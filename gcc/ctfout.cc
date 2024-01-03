/* Output CTF format from GCC.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "memmodel.h"
#include "tm_p.h"
#include "output.h"
#include "dwarf2asm.h"
#include "debug.h"
#include "ctfc.h"
#include "diagnostic-core.h"

static int ctf_label_num;

/* Pointers to various CTF sections.  */

static GTY (()) section * ctf_info_section;

/* Section names used to hold CTF debugging information.  */

/* CTF debug info section.  */

#ifndef CTF_INFO_SECTION_NAME
#define CTF_INFO_SECTION_NAME  ".ctf"
#endif

/* Section flags for the CTF debug info section.  */

#define CTF_INFO_SECTION_FLAGS (SECTION_DEBUG)

/* Maximum size (in bytes) of an artificially generated CTF label.  */

#define MAX_CTF_LABEL_BYTES 40

static char ctf_info_section_label[MAX_CTF_LABEL_BYTES];

#ifndef CTF_INFO_SECTION_LABEL
#define CTF_INFO_SECTION_LABEL			"Lctf"
#endif

/* CTF preprocess callback arguments.  */

typedef struct ctf_dtd_preprocess_arg
{
  uint64_t dtd_global_func_idx;
  ctf_container_ref dtd_arg_ctfc;
} ctf_dtd_preprocess_arg_t;

typedef struct ctf_dvd_preprocess_arg
{
  uint64_t dvd_global_obj_idx;
  ctf_container_ref dvd_arg_ctfc;
} ctf_dvd_preprocess_arg_t;

/* Compare two CTF variable definition entries.  Currently used for sorting
   by name.  */

static int
ctf_varent_compare (const void * entry1, const void * entry2)
{
  int result;
  const ctf_dvdef_t * e1 = *(const ctf_dvdef_t * const*) entry1;
  const ctf_dvdef_t * e2 = *(const ctf_dvdef_t * const*) entry2;

  result = strcmp (e1->dvd_name, e2->dvd_name);

  return result;
}

/* A CTF type record may be followed by variable-length of bytes to encode the
   CTF type completely.  This routine calculates the number of bytes, in the
   final binary CTF format, which are used to encode information about the type
   completely.

   This function must always be in sync with the CTF header.  */

static uint64_t
ctf_calc_num_vbytes (ctf_dtdef_ref ctftype)
{
  uint32_t size;
  uint64_t vlen_bytes = 0;

  uint32_t kind = CTF_V2_INFO_KIND (ctftype->dtd_data.ctti_info);
  uint32_t vlen = CTF_V2_INFO_VLEN (ctftype->dtd_data.ctti_info);

  ctf_dmdef_t * dmd;
  ctf_func_arg_t * farg;
  uint32_t size_per_member = 0;
  unsigned int num_members = 0;
  unsigned int num_fargs = 0;

  switch (kind)
    {
      case CTF_K_FORWARD:
      case CTF_K_UNKNOWN:
      case CTF_K_POINTER:
      case CTF_K_TYPEDEF:
      case CTF_K_VOLATILE:
      case CTF_K_CONST:
      case CTF_K_RESTRICT:
	/* These types have no vlen data.  */
	break;

      case CTF_K_INTEGER:
      case CTF_K_FLOAT:
	/* 4 bytes to represent encoding CTF_INT_DATA, CTF_FP_DATA.  */
	vlen_bytes += sizeof (uint32_t);
	break;
      case CTF_K_FUNCTION:
	/* Sanity check - number of function args must be the same as
	   vlen.  */
	for (farg = ctftype->dtd_u.dtu_argv;
	     farg != NULL; farg = (ctf_func_arg_t *) ctf_farg_list_next (farg))
	  num_fargs++;
	gcc_assert (vlen == num_fargs);

	/* FIXME - CTF_PADDING_FOR_ALIGNMENT.  */
	vlen_bytes += (vlen + (vlen & 1)) * sizeof (uint32_t);
	break;
      case CTF_K_ARRAY:
	/* This has a single ctf_array_t.  */
	vlen_bytes += sizeof (ctf_array_t);
	break;
      case CTF_K_SLICE:
	vlen_bytes += sizeof (ctf_slice_t);
	break;
      case CTF_K_STRUCT:
      case CTF_K_UNION:
	/* Count the number and type of members.  */
	size = ctftype->dtd_data.ctti_size;
	size_per_member = size >= CTF_LSTRUCT_THRESH
			  ? sizeof (ctf_lmember_t) : sizeof (ctf_member_t);

	/* Sanity check - number of members of struct must be the same as
	   vlen.  */
	for (dmd = ctftype->dtd_u.dtu_members;
	     dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
	  num_members++;
	gcc_assert (vlen == num_members);

	vlen_bytes += (num_members * size_per_member);
	break;
      case CTF_K_ENUM:
	vlen_bytes += vlen * sizeof (ctf_enum_t);
	break;
      default :
	break;
    }
  return vlen_bytes;
}

/* Add a CTF variable to the end of the list.  */

static void
ctf_list_add_ctf_vars (ctf_container_ref ctfc, ctf_dvdef_ref var)
{
  ctfc->ctfc_vars_list[ctfc->ctfc_vars_list_count++] = var;
}

/* Initialize the various sections and labels for CTF output.  */

void
init_ctf_sections (void)
{
  /* Note : Even in case of LTO, the compiler continues to generate a single
     CTF section for each compilation unit "early".  Unlike other debug
     sections, CTF sections are non-LTO sections, and do not take the
     .gnu.debuglto_ prefix.  The linker will de-duplicate the types in the CTF
     sections, in case of LTO or  otherwise.  */
  ctf_info_section = get_section (CTF_INFO_SECTION_NAME, CTF_INFO_SECTION_FLAGS,
				  NULL);

  ASM_GENERATE_INTERNAL_LABEL (ctf_info_section_label,
			       CTF_INFO_SECTION_LABEL, ctf_label_num++);
}

/* Routines for CTF pre-processing.  */

static void
ctf_preprocess_var (ctf_container_ref ctfc, ctf_dvdef_ref var)
{
  /* Add it to the list of types.  This array of types will be sorted before
     assembling into output.  */
  ctf_list_add_ctf_vars (ctfc, var);
}

/* CTF preprocess callback routine for CTF variables.  */

int
ctf_dvd_preprocess_cb (ctf_dvdef_ref * slot, void * arg)
{
  ctf_dvd_preprocess_arg_t * dvd_arg =  (ctf_dvd_preprocess_arg_t *)arg;
  ctf_dvdef_ref var = (ctf_dvdef_ref) *slot;
  ctf_container_ref arg_ctfc = dvd_arg->dvd_arg_ctfc;

  /* If the CTF variable corresponds to an extern variable declaration with
     a defining declaration later on, skip it.  Only CTF variable
     corresponding to the defining declaration for the extern variable is
     desirable.  */
  if (ctf_dvd_ignore_lookup (arg_ctfc, var->dvd_key))
    return 1;

  ctf_preprocess_var (arg_ctfc, var);

  /* Keep track of global objts.  */
  arg_ctfc->ctfc_gobjts_list[dvd_arg->dvd_global_obj_idx] = var;
  dvd_arg->dvd_global_obj_idx++;

  return 1;
}

/* CTF preprocess callback routine for CTF types.  */

int
ctf_dtd_preprocess_cb (ctf_dtdef_ref * slot, void * arg)
{
  uint32_t kind;

  ctf_dtdef_ref ctftype = (ctf_dtdef_ref) *slot;
  ctf_dtd_preprocess_arg_t * dtd_arg = (ctf_dtd_preprocess_arg_t *)arg;
  ctf_container_ref arg_ctfc = dtd_arg->dtd_arg_ctfc;

  size_t index = ctftype->dtd_type;
  gcc_assert (index <= arg_ctfc->ctfc_types->elements ());

  /* CTF types need to be output in the order of their type IDs.  In other
     words, if type A is used to define type B, type ID of type A must
     appear before type ID of type B.  */
  arg_ctfc->ctfc_types_list[index] = ctftype;

  /* Keep track of the CTF type if it's a function type and the type
     was generated from a function object.  */
  kind = CTF_V2_INFO_KIND (ctftype->dtd_data.ctti_info);
  if (kind == CTF_K_FUNCTION && ctftype->from_global_func)
    {
      arg_ctfc->ctfc_gfuncs_list[dtd_arg->dtd_global_func_idx] = ctftype;
      dtd_arg->dtd_global_func_idx++;
    }

  /* Calculate the vlen bytes.  */
  arg_ctfc->ctfc_num_vlen_bytes += ctf_calc_num_vbytes (ctftype);

  return 1;
}

/* CTF preprocessing.
   After the CTF types for the compilation unit have been generated fully, the
   compiler writes out the asm for the CTF types.

   CTF writeout in the compiler requires two passes over the CTF types.  In the
   first pass, the CTF preprocess pass:
     1.  CTF types are sorted in the order of their type IDs.
     2.  The variable number of bytes after each CTF type record are calculated.
	 This is used to calculate the offsets in the ctf_header_t.
     3.  If the CTF type is of CTF_K_FUNCTION, the number of bytes in the
	 funcinfo sub-section are calculated.  This is used to calculate the
	 offsets in the ctf_header_t.
     4.  Keep the list of CTF variables in ASCIIbetical order of their names.

   In the second pass, the CTF writeout pass, asm tags are written out using
   the compiler's afore-generated internal pre-processed CTF types.  */

static void
ctf_preprocess (ctf_container_ref ctfc)
{
  size_t num_ctf_types = ctfc->ctfc_types->elements ();
  size_t num_ctf_vars = ctfc_get_num_ctf_vars (ctfc);

  /* Initialize an array to keep track of the CTF variables at global
     scope.  At this time, size it conservatively.  */
  size_t num_global_objts = num_ctf_vars;
  if (num_global_objts)
    {
      ctfc->ctfc_gobjts_list = ggc_vec_alloc<ctf_dvdef_t*>(num_global_objts);
    }

  if (num_ctf_vars)
    {
      ctf_dvd_preprocess_arg_t dvd_arg;
      dvd_arg.dvd_global_obj_idx = 0;
      dvd_arg.dvd_arg_ctfc = ctfc;

      /* Allocate CTF var list.  */
      ctfc->ctfc_vars_list = ggc_vec_alloc<ctf_dvdef_ref>(num_ctf_vars);
      /* Variables appear in the sort ASCIIbetical order of their names.  This
	 permits binary searching in the CTF reader.  Add the variables to a
	 list for sorting.  */
      ctfc->ctfc_vars->traverse<void *, ctf_dvd_preprocess_cb> (&dvd_arg);
      /* Sort the list.  */
      qsort (ctfc->ctfc_vars_list, ctfc->ctfc_vars_list_count,
	     sizeof (ctf_dvdef_ref), ctf_varent_compare);
      /* Update the actual number of the generated CTF variables at global
	 scope.  */
      ctfc->ctfc_num_global_objts = dvd_arg.dvd_global_obj_idx;
    }

  /* Initialize an array to keep track of the CTF functions types for global
     functions in the CTF data section.  */
  size_t num_global_funcs = ctfc->ctfc_num_global_funcs;
  if (num_global_funcs)
    {
      ctfc->ctfc_gfuncs_list = ggc_vec_alloc<ctf_dtdef_t*>(num_global_funcs);
      gcc_assert (num_ctf_types);
    }

  if (num_ctf_types)
    {
      ctf_dtd_preprocess_arg_t dtd_arg;
      dtd_arg.dtd_global_func_idx = 0;
      dtd_arg.dtd_arg_ctfc = ctfc;
      /* Allocate the CTF types list.  Add 1 because type ID 0 is never a valid
	 CTF type ID.  No CTF type record should appear at that offset, this
	 eases debugging and readability.  */
      ctfc->ctfc_types_list = ggc_vec_alloc<ctf_dtdef_ref>(num_ctf_types + 1);
      /* Pre-process CTF types.  */
      ctfc->ctfc_types->traverse<void *, ctf_dtd_preprocess_cb> (&dtd_arg);

      gcc_assert (dtd_arg.dtd_global_func_idx == num_global_funcs);
    }
}

/* CTF asm helper routines.  */

/* Asm'out the CTF preamble.  */

static void
ctf_asm_preamble (ctf_container_ref ctfc)
{
  dw2_asm_output_data (2, ctfc->ctfc_magic,
		       "CTF preamble magic number");
  dw2_asm_output_data (1, ctfc->ctfc_version, "CTF preamble version");
  dw2_asm_output_data (1, ctfc->ctfc_flags, "CTF preamble flags");
}

/* Asm'out a CTF type which is represented by ctf_stype_t.  */

static void
ctf_asm_stype (ctf_dtdef_ref type)
{
  dw2_asm_output_data (4, type->dtd_data.ctti_name, "ctt_name");
  dw2_asm_output_data (4, type->dtd_data.ctti_info, "ctt_info");
  /* union.  */
  dw2_asm_output_data (4, type->dtd_data.ctti_size, "ctt_size or ctt_type");
}

/* Asm'out a CTF type which is represented by ctf_type_t.  */

static void
ctf_asm_type (ctf_dtdef_ref type)
{
  dw2_asm_output_data (4, type->dtd_data.ctti_name, "ctt_name");
  dw2_asm_output_data (4, type->dtd_data.ctti_info, "ctt_info");
  /* union.  */
  dw2_asm_output_data (4, type->dtd_data.ctti_size, "ctt_size");
  dw2_asm_output_data (4, type->dtd_data.ctti_lsizehi, "ctt_lsizehi");
  dw2_asm_output_data (4, type->dtd_data.ctti_lsizelo, "ctt_lsizelo");
}

/* Asm'out a CTF type of kind CTF_K_SLICE.  */

static void
ctf_asm_slice (ctf_dtdef_ref type)
{
  dw2_asm_output_data (4, type->dtd_u.dtu_slice.cts_type, "cts_type");
  dw2_asm_output_data (2, type->dtd_u.dtu_slice.cts_offset, "cts_offset");
  dw2_asm_output_data (2, type->dtd_u.dtu_slice.cts_bits, "cts_bits");
}

/* Asm'out a CTF type of kind CTF_K_ARRAY.  */

static void
ctf_asm_array (ctf_dtdef_ref dtd)
{
  dw2_asm_output_data (4, dtd->dtd_u.dtu_arr.ctr_contents, "cta_contents");
  dw2_asm_output_data (4, dtd->dtd_u.dtu_arr.ctr_index, "cta_index");
  dw2_asm_output_data (4, dtd->dtd_u.dtu_arr.ctr_nelems, "cta_nelems");
}

/* Asm'out a CTF variable.  */

static void
ctf_asm_varent (ctf_dvdef_ref var)
{
  /* Output the reference to the name in the string table.  */
  dw2_asm_output_data (4, var->dvd_name_offset, "ctv_name");
  /* Output the type index.  */
  dw2_asm_output_data (4, var->dvd_type, "ctv_typeidx");
}

/* Asm'out a member of CTF struct or union, represented by ctf_lmember_t.  */

static void
ctf_asm_sou_lmember (ctf_dmdef_t * dmd)
{
  dw2_asm_output_data (4, dmd->dmd_name_offset, "ctlm_name");
  dw2_asm_output_data (4, CTF_OFFSET_TO_LMEMHI (dmd->dmd_offset),
		       "ctlm_offsethi");
  dw2_asm_output_data (4, dmd->dmd_type, "ctlm_type");
  dw2_asm_output_data (4, CTF_OFFSET_TO_LMEMLO (dmd->dmd_offset),
		       "ctlm_offsetlo");
}

/* Asm'out a member of a CTF sruct or union, represented by ctf_member_t.  */

static void
ctf_asm_sou_member (ctf_dmdef_t * dmd)
{
  dw2_asm_output_data (4, dmd->dmd_name_offset, "ctm_name");
  dw2_asm_output_data (4, dmd->dmd_offset, "ctm_offset");
  dw2_asm_output_data (4, dmd->dmd_type, "ctm_type");
}

/* Asm'out an enumerator constant.  */

static void
ctf_asm_enum_const (ctf_dmdef_t * dmd)
{
  dw2_asm_output_data (4, dmd->dmd_name_offset, "cte_name");
  dw2_asm_output_data (4, dmd->dmd_value, "cte_value");
}

/* Asm'out a function argument.  */

static void
ctf_asm_func_arg (ctf_func_arg_t * farg)
{
  dw2_asm_output_data (4, farg->farg_type, "dtu_argv");
}

/* CTF writeout to asm file.  */

static void
output_ctf_header (ctf_container_ref ctfc)
{
  switch_to_section (ctf_info_section);
  ASM_OUTPUT_LABEL (asm_out_file, ctf_info_section_label);

  ctf_asm_preamble (ctfc);

  /* For a single compilation unit, the parent container's name and label are
     NULL.  */
  dw2_asm_output_data (4, 0, "cth_parlabel");
  dw2_asm_output_data (4, 0, "cth_parname");
  dw2_asm_output_data (4, ctfc->ctfc_cuname_offset, "cth_cuname");

  int typeslen = 0;
  /* Initialize the offsets.  The offsets are from after the CTF header.  */
  uint32_t lbloff = 0;
  uint32_t objtoff = 0;
  uint32_t funcoff = 0;
  uint32_t objtidxoff = 0;
  uint32_t funcidxoff = 0;
  uint32_t varoff = 0;
  uint32_t typeoff = 0;
  uint32_t stroff = 0;

  if (!ctfc_is_empty_container (ctfc))
    {
      gcc_assert (ctfc_get_num_ctf_types (ctfc)
		  == (ctfc->ctfc_num_types + ctfc->ctfc_num_stypes));

      funcoff = objtoff + ctfc->ctfc_num_global_objts * sizeof (uint32_t);
      /* Object index appears after function info.  */
      objtidxoff = funcoff + ctfc->ctfc_num_global_funcs * sizeof (uint32_t);
      /* Funxtion index goes next.  */
      funcidxoff = objtidxoff + ctfc->ctfc_num_global_objts * sizeof (uint32_t);
      /* Vars appear after function index.  */
      varoff = funcidxoff + ctfc->ctfc_num_global_funcs * sizeof (uint32_t);
      /* CTF types appear after vars.  */
      typeoff = varoff + (ctfc->ctfc_vars_list_count) * sizeof (ctf_varent_t);
      /* The total number of bytes for CTF types is the sum of the number of
	 times struct ctf_type_t, struct ctf_stype_t are written, plus the
	 amount of variable length data after each one of these.  */
      typeslen = ctfc->ctfc_num_types * sizeof (ctf_type_t)
		+ ctfc->ctfc_num_stypes * (sizeof (ctf_stype_t))
		+ ctfc_get_num_vlen_bytes (ctfc);

      /* Strings appear after types.  */
      stroff = typeoff + typeslen;
    }

    /* Offset of label section.  */
    dw2_asm_output_data (4, lbloff, "cth_lbloff");
    /* Offset of object section.  */
    dw2_asm_output_data (4, objtoff, "cth_objtoff");
    /* Offset of function section.  */
    dw2_asm_output_data (4, funcoff, "cth_funcoff");
    /* Offset of object index section.  */
    dw2_asm_output_data (4, objtidxoff, "cth_objtidxoff");
    /* Offset of function index section.  */
    dw2_asm_output_data (4, funcidxoff, "cth_funcidxoff");

    /* Offset of variable section.  */
    dw2_asm_output_data (4, varoff, "cth_varoff");
    /* Offset of type section.  */
    dw2_asm_output_data (4, typeoff, "cth_typeoff");
    /* Offset of string section.  */
    dw2_asm_output_data (4, stroff, "cth_stroff");
    /* Length of string section in bytes.  */
    dw2_asm_output_data (4, ctfc->ctfc_strlen, "cth_strlen");
}

/* Output the CTF object info section.  */

static void
output_ctf_obj_info (ctf_container_ref ctfc)
{
  uint64_t i;
  ctf_dvdef_ref var;

  if (!ctfc->ctfc_num_global_objts) return;

  /* Compiler spits out the objts (at global scope) in the CTF obj info section.
     In no specific order.  In an object file, the CTF object index section is
     used to associate the objts to their corresponding names.  */
  for (i = 0; i < ctfc->ctfc_num_global_objts; i++)
    {
      var = ctfc->ctfc_gobjts_list[i];

      /* CTF type ID corresponding to the type of the variable.  */
      dw2_asm_output_data (4, var->dvd_type, "objtinfo_var_type");
    }

}

/* Output the CTF function info section.  */

static void
output_ctf_func_info (ctf_container_ref ctfc)
{
  uint64_t i;
  ctf_dtdef_ref ctftype;

  if (!ctfc->ctfc_num_global_funcs) return;

  /* The CTF funcinfo section is simply an array of CTF_K_FUNCTION type IDs in
     the type section.  In an object file, the CTF function index section is
     used to associate functions to their corresponding names.  */
  for (i = 0; i < ctfc->ctfc_num_global_funcs; i++)
    {
      ctftype = ctfc->ctfc_gfuncs_list[i];
      dw2_asm_output_data (4, ctftype->dtd_type, "funcinfo_func_type");
    }
}

/* Output the CTF object index section.  */

static void
output_ctf_objtidx (ctf_container_ref ctfc)
{
  uint64_t i;
  ctf_dvdef_ref var;

  if (!ctfc->ctfc_num_global_objts) return;

  for (i = 0; i < ctfc->ctfc_num_global_objts; i++)
    {
      var = ctfc->ctfc_gobjts_list[i];
      /* Offset to the name in CTF string table.  */
      dw2_asm_output_data (4, var->dvd_name_offset, "objtinfo_name");
    }
}

/* Output the CTF function index section.  */

static void
output_ctf_funcidx (ctf_container_ref ctfc)
{
  uint64_t i;
  ctf_dtdef_ref ctftype;

  if (!ctfc->ctfc_num_global_funcs) return;

  for (i = 0; i < ctfc->ctfc_num_global_funcs; i++)
    {
      ctftype = ctfc->ctfc_gfuncs_list[i];
      /* Offset to the name in CTF string table.  */
      dw2_asm_output_data (4, ctftype->dtd_data.ctti_name, "funcinfo_name");
    }
}

/* Output the CTF variables.  Variables appear in the sorted ASCIIbetical
   order of their names.  This permits binary searching in the CTF reader.  */

static void
output_ctf_vars (ctf_container_ref ctfc)
{
  size_t i;
  unsigned int num_ctf_vars = ctfc->ctfc_vars_list_count;
  if (num_ctf_vars)
    {
      /* Iterate over the list of sorted vars and output the asm.  */
      for (i = 0; i < num_ctf_vars; i++)
	{
	  ctf_asm_varent (ctfc->ctfc_vars_list[i]);
	  /* The type of variable must be a valid one.  */
	  gcc_assert (ctfc->ctfc_vars_list[i]->dvd_type != CTF_NULL_TYPEID);
	}
    }
}

/* Output the CTF string records.  */

static void
output_ctf_strs (ctf_container_ref ctfc)
{
  ctf_string_t * ctf_string = ctfc->ctfc_strtable.ctstab_head;

  while (ctf_string)
    {
      dw2_asm_output_nstring (ctf_string->cts_str, -1, "ctf_string");
      ctf_string = ctf_string->cts_next;
    }
}

/* Output the members of the CTF struct or union.  */

static void
output_asm_ctf_sou_fields (ctf_container_ref ARG_UNUSED (ctfc),
			   ctf_dtdef_ref dtd)
{
  ctf_dmdef_t * dmd;

  /* Function pointer to dump struct/union members.  */
  void (*ctf_asm_sou_field_func) (ctf_dmdef_t *);

  uint32_t size = dtd->dtd_data.ctti_size;

  /* The variable length data struct/union CTF types is an array of
     ctf_member or ctf_lmember, depending on size of the member.  */
  if (size >= CTF_LSTRUCT_THRESH)
    ctf_asm_sou_field_func = ctf_asm_sou_lmember;
  else
    ctf_asm_sou_field_func = ctf_asm_sou_member;

  for (dmd = dtd->dtd_u.dtu_members;
       dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
    {
      ctf_asm_sou_field_func (dmd);
      /* Sanity Check - Unrepresented types appear as explicit types.  */
      gcc_assert (dmd->dmd_type != CTF_NULL_TYPEID);
    }
}

/* Output the list of enumerator constants of the CTF enum type.  */

static void
output_asm_ctf_enum_list (ctf_container_ref ARG_UNUSED (ctfc),
			  ctf_dtdef_ref dtd)
{
  ctf_dmdef_t * dmd;

  for (dmd = dtd->dtd_u.dtu_members;
       dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
    ctf_asm_enum_const (dmd);
}

/* Output the list of function arguments of the CTF function type.  */

static void
output_asm_func_args_list (ctf_container_ref ARG_UNUSED (ctfc),
			  ctf_dtdef_ref dtd)
{
  ctf_func_arg_t * farg;

  for (farg = dtd->dtd_u.dtu_argv;
       farg != NULL; farg = (ctf_func_arg_t *) ctf_farg_list_next (farg))
    ctf_asm_func_arg (farg);
}

/* Output the variable length portion of the CTF type record.  */

static void
output_asm_ctf_vlen_bytes (ctf_container_ref ctfc, ctf_dtdef_ref ctftype)
{
  uint32_t encoding;
  uint32_t kind = CTF_V2_INFO_KIND (ctftype->dtd_data.ctti_info);
  uint32_t vlen = CTF_V2_INFO_VLEN (ctftype->dtd_data.ctti_info);

  switch (kind)
    {
      case CTF_K_INTEGER:
      case CTF_K_FLOAT:
	if (kind == CTF_K_INTEGER)
	  {
	    encoding = CTF_INT_DATA (ctftype->dtd_u.dtu_enc.cte_format,
				     ctftype->dtd_u.dtu_enc.cte_offset,
				     ctftype->dtd_u.dtu_enc.cte_bits);
	  }
	else
	  {
	    encoding = CTF_FP_DATA (ctftype->dtd_u.dtu_enc.cte_format,
				    ctftype->dtd_u.dtu_enc.cte_offset,
				    ctftype->dtd_u.dtu_enc.cte_bits);
	  }
	dw2_asm_output_data (4, encoding, "ctf_encoding_data");
	break;
      case CTF_K_FUNCTION:
	  {
	    output_asm_func_args_list (ctfc, ctftype);
	    /* FIXME - CTF_PADDING_FOR_ALIGNMENT.
	       libctf expects this padding for alignment reasons.  Expected to
	       be redundant in CTF_VERSION_4.  */
	    if (vlen & 1)
	      dw2_asm_output_data (4, 0, "dtu_argv_padding");

	    break;
	  }
      case CTF_K_ARRAY:
	ctf_asm_array (ctftype);
	break;
      case CTF_K_SLICE:
	  {
	    ctf_asm_slice (ctftype);
	    /* Type of the slice must be a valid CTF type.  */
	    gcc_assert (ctftype->dtd_u.dtu_slice.cts_type != CTF_NULL_TYPEID);
	    break;
	  }
      case CTF_K_STRUCT:
      case CTF_K_UNION:
	output_asm_ctf_sou_fields (ctfc, ctftype);
	break;
      case CTF_K_ENUM:
	output_asm_ctf_enum_list (ctfc, ctftype);
	break;

      default:
	/* CTF types of kind CTF_K_VOLATILE, CTF_K_CONST, CTF_K_RESTRICT,
	   etc have no vlen data to write.  */
	break;
    }
}

/* Output a CTF Type.  */

static void
output_asm_ctf_type (ctf_container_ref ctfc, ctf_dtdef_ref type)
{
  if (type->dtd_data.ctti_size <= CTF_MAX_SIZE)
    ctf_asm_stype (type);
  else
    ctf_asm_type (type);
  /* Now comes the variable-length portion for defining types completely.
     E.g., encoding follows CTF_INT_DATA, CTF_FP_DATA types,
     struct ctf_array_t follows CTF_K_ARRAY types, or a bunch of
     struct ctf_member / ctf_lmember ctf_enum sit in there for CTF_K_STRUCT or
     CTF_K_UNION.  */
  output_asm_ctf_vlen_bytes (ctfc, type);

  uint32_t kind = CTF_V2_INFO_KIND (type->dtd_data.ctti_info);
  /* The underlying type must be a valid CTF type.  */
  if (kind == CTF_K_POINTER || kind == CTF_K_TYPEDEF
      || kind == CTF_K_VOLATILE || kind == CTF_K_CONST
      || kind == CTF_K_RESTRICT)
    gcc_assert (type->dtd_data.ctti_type != CTF_NULL_TYPEID);
}

/* Output all CTF type records.  */

static void
output_ctf_types (ctf_container_ref ctfc)
{
  size_t i;
  size_t num_ctf_types = ctfc->ctfc_types->elements ();
  if (num_ctf_types)
    {
      /* Type ID = 0 is used as sentinel value; not a valid type.  */
      for (i = 1; i <= num_ctf_types; i++)
	output_asm_ctf_type (ctfc, ctfc->ctfc_types_list[i]);
    }
}

/* CTF routines interfacing to the compiler.  */

/* Prepare and output the CTF section.  */

void
ctf_output (const char * filename)
{
  if (ctf_debug_info_level == CTFINFO_LEVEL_NONE)
    return;

  /* Get the CTF container for the current translation unit.  */
  ctf_container_ref tu_ctfc = ctf_get_tu_ctfc ();

  init_ctf_sections ();

  ctf_add_cuname (tu_ctfc, filename);

  /* Pre-process CTF before generating assembly.  */
  ctf_preprocess (tu_ctfc);
  output_ctf_header (tu_ctfc);
  output_ctf_obj_info (tu_ctfc);
  output_ctf_func_info (tu_ctfc);
  output_ctf_objtidx (tu_ctfc);
  output_ctf_funcidx (tu_ctfc);
  output_ctf_vars (tu_ctfc);
  output_ctf_types (tu_ctfc);
  output_ctf_strs (tu_ctfc);

  /* The total number of string bytes must be equal to those processed out to
     the str subsection.  */
  gcc_assert (tu_ctfc->ctfc_strlen
	      == ctfc_get_strtab_len (tu_ctfc, CTF_STRTAB));

}

/* Reset all state for CTF generation so that we can rerun the compiler within
   the same process.  */

void
ctf_finalize (void)
{
  ctf_info_section = NULL;

  ctf_container_ref tu_ctfc = ctf_get_tu_ctfc ();
  ctfc_delete_container (tu_ctfc);
  tu_ctfc = NULL;
}

#include "gt-ctfout.h"
