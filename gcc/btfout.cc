/* Output BTF format from GCC.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

/* This file contains routines to output the BPF Type Format (BTF). The BTF
   debug format is very similar to CTF; as a result, the structure of this file
   closely resembles that of ctfout.cc, and the same CTF container objects are
   used.  */

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
#include "cgraph.h"
#include "varasm.h"
#include "dwarf2out.h" /* For lookup_decl_die.  */

static int btf_label_num;

static GTY (()) section * btf_info_section;

/* BTF debug info section.  */

#ifndef BTF_INFO_SECTION_NAME
#define BTF_INFO_SECTION_NAME  ".BTF"
#endif

#define BTF_INFO_SECTION_FLAGS (SECTION_DEBUG)

/* Maximum size (in bytes) for an artifically generated BTF label.  */

#define MAX_BTF_LABEL_BYTES 40

static char btf_info_section_label[MAX_BTF_LABEL_BYTES];

#ifndef BTF_INFO_SECTION_LABEL
#define BTF_INFO_SECTION_LABEL  "Lbtf"
#endif

/* BTF encodes void as type id 0.  */

#define BTF_VOID_TYPEID 0
#define BTF_INIT_TYPEID 1

#define BTF_INVALID_TYPEID 0xFFFFFFFF

/* Mapping of CTF variables to the IDs they will be assigned when they are
   converted to BTF_KIND_VAR type records. Strictly accounts for the index
   from the start of the variable type entries, does not include the number
   of types emitted prior to the variable records.  */
static GTY (()) hash_map <ctf_dvdef_ref, unsigned> *btf_var_ids;

/* Mapping of type IDs from original CTF ID to BTF ID. Types do not map
   1-to-1 from CTF to BTF. To avoid polluting the CTF container when updating
   type references-by-ID, we use this map instead.  */
static ctf_id_t * btf_id_map = NULL;

/* Information for creating the BTF_KIND_DATASEC records.  */
typedef struct btf_datasec
{
  const char *name;                    /* Section name, e.g. ".bss".  */
  uint32_t name_offset;                /* Offset to name in string table.  */
  vec<struct btf_var_secinfo> entries; /* Variable entries in this section.  */
} btf_datasec_t;

/* One BTF_KIND_DATASEC record is created for each output data section which
   will hold at least one variable.  */
static vec<btf_datasec_t> datasecs;

/* Holes occur for types which are present in the CTF container, but are either
   non-representable or redundant in BTF.  */
static vec<ctf_id_t> holes;

/* CTF definition(s) of void. Only one definition of void should be generated.
   We should not encounter more than one definition of void, but use a vector
   to be safe.  */
static vec<ctf_id_t> voids;

/* Functions in BTF have two separate type records - one for the prototype
   (BTF_KIND_FUNC_PROTO), as well as a BTF_KIND_FUNC. CTF_K_FUNCTION types
   map closely to BTF_KIND_FUNC_PROTO, but the BTF_KIND_FUNC records must be
   created. This vector holds them.  */
static GTY (()) vec<ctf_dtdef_ref, va_gc> *funcs;

/* The number of BTF variables added to the TU CTF container.  */
static unsigned int num_vars_added = 0;

/* The number of BTF types added to the TU CTF container.  */
static unsigned int num_types_added = 0;

/* The number of types synthesized for BTF that do not correspond to
   CTF types.  */
static unsigned int num_types_created = 0;

/* Map a CTF type kind to the corresponding BTF type kind.  */

static uint32_t
get_btf_kind (uint32_t ctf_kind)
{
  /* N.B. the values encoding kinds are not in general the same for the
     same kind between CTF and BTF. e.g. CTF_K_CONST != BTF_KIND_CONST.  */
  switch (ctf_kind)
    {
    case CTF_K_INTEGER:  return BTF_KIND_INT;
    case CTF_K_FLOAT:	 return BTF_KIND_FLOAT;
    case CTF_K_POINTER:  return BTF_KIND_PTR;
    case CTF_K_ARRAY:    return BTF_KIND_ARRAY;
    case CTF_K_FUNCTION: return BTF_KIND_FUNC_PROTO;
    case CTF_K_STRUCT:   return BTF_KIND_STRUCT;
    case CTF_K_UNION:    return BTF_KIND_UNION;
    case CTF_K_ENUM:     return BTF_KIND_ENUM;
    case CTF_K_FORWARD:  return BTF_KIND_FWD;
    case CTF_K_TYPEDEF:  return BTF_KIND_TYPEDEF;
    case CTF_K_VOLATILE: return BTF_KIND_VOLATILE;
    case CTF_K_CONST:    return BTF_KIND_CONST;
    case CTF_K_RESTRICT: return BTF_KIND_RESTRICT;
    default:;
    }
  return BTF_KIND_UNKN;
}

/* Allocate the btf_id_map, and initialize elements to BTF_INVALID_TYPEID.  */

static void
init_btf_id_map (size_t len)
{
  btf_id_map = XNEWVEC (ctf_id_t, len);

  btf_id_map[0] = BTF_VOID_TYPEID;
  for (size_t i = 1; i < len; i++)
    btf_id_map[i] = BTF_INVALID_TYPEID;
}

/* Return the BTF type ID of CTF type ID KEY, or BTF_INVALID_TYPEID if the CTF
   type with ID KEY does not map to a BTF type.  */

ctf_id_t
get_btf_id (ctf_id_t key)
{
  return btf_id_map[key];
}

/* Set the CTF type ID KEY to map to BTF type ID VAL.  */

static inline void
set_btf_id (ctf_id_t key, ctf_id_t val)
{
  btf_id_map[key] = val;
}

/* Return TRUE iff the given CTF type ID maps to a BTF type which will
   be emitted.  */
static inline bool
btf_emit_id_p (ctf_id_t id)
{
  return ((btf_id_map[id] != BTF_VOID_TYPEID)
	  && (btf_id_map[id] <= BTF_MAX_TYPE));
}

/* Each BTF type can be followed additional, variable-length information
   completing the description of the type. Calculate the number of bytes
   of variable information required to encode a given type.  */

static uint64_t
btf_calc_num_vbytes (ctf_dtdef_ref dtd)
{
  uint64_t vlen_bytes = 0;

  uint32_t kind = get_btf_kind (CTF_V2_INFO_KIND (dtd->dtd_data.ctti_info));
  uint32_t vlen = CTF_V2_INFO_VLEN (dtd->dtd_data.ctti_info);

  switch (kind)
    {
    case BTF_KIND_UNKN:
    case BTF_KIND_PTR:
    case BTF_KIND_FWD:
    case BTF_KIND_TYPEDEF:
    case BTF_KIND_VOLATILE:
    case BTF_KIND_CONST:
    case BTF_KIND_RESTRICT:
    case BTF_KIND_FUNC:
    /* These kinds have no vlen data.  */
      break;

    case BTF_KIND_INT:
      /* Size 0 integers represent redundant definitions of void that will
	 not be emitted. Don't allocate space for them.  */
      if (dtd->dtd_data.ctti_size == 0)
	break;

      vlen_bytes += sizeof (uint32_t);
      break;

    case BTF_KIND_ARRAY:
      vlen_bytes += sizeof (struct btf_array);
      break;

    case BTF_KIND_STRUCT:
    case BTF_KIND_UNION:
      vlen_bytes += vlen * sizeof (struct btf_member);
      break;

    case BTF_KIND_ENUM:
      vlen_bytes += (dtd->dtd_data.ctti_size == 0x8)
			? vlen * sizeof (struct btf_enum64)
			: vlen * sizeof (struct btf_enum);
      break;

    case BTF_KIND_FUNC_PROTO:
      vlen_bytes += vlen * sizeof (struct btf_param);
      break;

    case BTF_KIND_VAR:
      vlen_bytes += sizeof (struct btf_var);
      break;

    case BTF_KIND_DATASEC:
      vlen_bytes += vlen * sizeof (struct btf_var_secinfo);
      break;

    default:
      break;
    }
  return vlen_bytes;
}

/* Initialize BTF section (.BTF) for output.  */

void
init_btf_sections (void)
{
  btf_info_section = get_section (BTF_INFO_SECTION_NAME, BTF_INFO_SECTION_FLAGS,
				  NULL);

  ASM_GENERATE_INTERNAL_LABEL (btf_info_section_label,
			       BTF_INFO_SECTION_LABEL, btf_label_num++);
}

/* Push a BTF datasec variable entry INFO into the datasec named SECNAME,
   creating the datasec if it does not already exist.  */

static void
btf_datasec_push_entry (ctf_container_ref ctfc, const char *secname,
			struct btf_var_secinfo info)
{
  if (secname == NULL)
    return;

  for (size_t i = 0; i < datasecs.length (); i++)
    if (strcmp (datasecs[i].name, secname) == 0)
      {
	datasecs[i].entries.safe_push (info);
	return;
      }

  /* If we don't already have a datasec record for secname, make one.  */

  uint32_t str_off;
  ctf_add_string (ctfc, secname, &str_off, CTF_AUX_STRTAB);
  if (strcmp (secname, ""))
    ctfc->ctfc_aux_strlen += strlen (secname) + 1;

  btf_datasec_t ds;
  ds.name = secname;
  ds.name_offset = str_off;

  ds.entries.create (0);
  ds.entries.safe_push (info);

  datasecs.safe_push (ds);
}


/* Return the section name, as of interest to btf_collect_datasec, for the
   given symtab node.  Note that this deliberately returns NULL for objects
   which do not go in a section btf_collect_datasec cares about.  */
static const char *
get_section_name (symtab_node *node)
{
  const char *section_name = node->get_section ();

  if (section_name == NULL)
    {
      switch (categorize_decl_for_section (node->decl, 0))
	{
	case SECCAT_BSS:
	  section_name = ".bss";
	  break;
	case SECCAT_DATA:
	  section_name = ".data";
	  break;
	case SECCAT_RODATA:
	  section_name = ".rodata";
	  break;
	default:;
	}
    }

  return section_name;
}

/* Construct all BTF_KIND_DATASEC records for CTFC. One such record is created
   for each non-empty data-containing section in the output. Each record is
   followed by a variable number of entries describing the variables stored
   in that section.  */

static void
btf_collect_datasec (ctf_container_ref ctfc)
{
  cgraph_node *func;
  FOR_EACH_FUNCTION (func)
    {
      dw_die_ref die = lookup_decl_die (func->decl);
      if (die == NULL)
	continue;

      ctf_dtdef_ref dtd = ctf_dtd_lookup (ctfc, die);
      if (dtd == NULL)
	continue;

      /* Functions actually get two types: a BTF_KIND_FUNC_PROTO, and
	 also a BTF_KIND_FUNC.  But the CTF container only allocates one
	 type per function, which matches closely with BTF_KIND_FUNC_PROTO.
	 For each such function, also allocate a BTF_KIND_FUNC entry.
	 These will be output later.  */
      ctf_dtdef_ref func_dtd = ggc_cleared_alloc<ctf_dtdef_t> ();
      func_dtd->dtd_data = dtd->dtd_data;
      func_dtd->dtd_data.ctti_type = dtd->dtd_type;
      func_dtd->linkage = dtd->linkage;
      func_dtd->dtd_type = num_types_added + num_types_created;

      /* Only the BTF_KIND_FUNC type actually references the name. The
	 BTF_KIND_FUNC_PROTO is always anonymous.  */
      dtd->dtd_data.ctti_name = 0;

      vec_safe_push (funcs, func_dtd);
      num_types_created++;

      /* Mark any 'extern' funcs and add DATASEC entries for them.  */
      if (DECL_EXTERNAL (func->decl))
	{
	  func_dtd->linkage = BTF_FUNC_EXTERN;

	  const char *section_name = get_section_name (func);
	  /* Note: get_section_name () returns NULL for functions in text
	     section.  This is intentional, since we do not want to generate
	     DATASEC entries for them.  */
	  if (section_name == NULL)
	    continue;

	  struct btf_var_secinfo info;

	  /* +1 for the sentinel type not in the types map.  */
	  info.type = func_dtd->dtd_type + 1;

	  /* Both zero at compile time.  */
	  info.size = 0;
	  info.offset = 0;

	  btf_datasec_push_entry (ctfc, section_name, info);
	}
    }

  varpool_node *node;
  FOR_EACH_VARIABLE (node)
    {
      dw_die_ref die = lookup_decl_die (node->decl);
      if (die == NULL)
	continue;

      ctf_dvdef_ref dvd = ctf_dvd_lookup (ctfc, die);
      if (dvd == NULL)
	continue;

      /* Mark extern variables.  */
      if (DECL_EXTERNAL (node->decl))
	dvd->dvd_visibility = BTF_VAR_GLOBAL_EXTERN;

      const char *section_name = get_section_name (node);
      if (section_name == NULL)
	continue;

      struct btf_var_secinfo info;

      info.type = 0;
      unsigned int *var_id = btf_var_ids->get (dvd);
      if (var_id)
	/* +1 for the sentinel type not in the types map.  */
	info.type = *var_id + num_types_added + 1;
      else
	continue;

      info.size = 0;
      tree size = DECL_SIZE_UNIT (node->decl);
      if (tree_fits_uhwi_p (size))
	info.size = tree_to_uhwi (size);
      else if (VOID_TYPE_P (TREE_TYPE (node->decl)))
	info.size = 1;

      /* Offset is left as 0 at compile time, to be filled in by loaders such
	 as libbpf.  */
      info.offset = 0;

      btf_datasec_push_entry (ctfc, section_name, info);
    }

  num_types_created += datasecs.length ();
}

/* Return true if the type ID is that of a type which will not be emitted (for
   example, if it is not representable in BTF).  */

static bool
btf_removed_type_p (ctf_id_t id)
{
  return holes.contains (id);
}

/* Adjust the given type ID to account for holes and duplicate definitions of
   void.  */

static ctf_id_t
btf_adjust_type_id (ctf_id_t id)
{
  size_t n;
  ctf_id_t i = 0;

  /* Do not adjust invalid type markers.  */
  if (id == BTF_INVALID_TYPEID)
    return id;

  for (n = 0; n < voids.length (); n++)
    if (id == voids[n])
      return BTF_VOID_TYPEID;

  for (n = 0; n < holes.length (); n++)
    {
      if (holes[n] < id)
	i++;
      else if (holes[n] == id)
	return BTF_VOID_TYPEID;
    }

  return id - i;
}

/* Postprocessing callback routine for types.  */

int
btf_dtd_postprocess_cb (ctf_dtdef_ref *slot, ctf_container_ref arg_ctfc)
{
  ctf_dtdef_ref ctftype = (ctf_dtdef_ref) * slot;

  size_t index = ctftype->dtd_type;
  gcc_assert (index <= arg_ctfc->ctfc_types->elements ());

  uint32_t ctf_kind, btf_kind;

  ctf_kind = CTF_V2_INFO_KIND (ctftype->dtd_data.ctti_info);
  btf_kind = get_btf_kind (ctf_kind);

  if (btf_kind == BTF_KIND_UNKN)
    /* This type is not representable in BTF. Create a hole.  */
    holes.safe_push (ctftype->dtd_type);

  else if (btf_kind == BTF_KIND_INT && ctftype->dtd_data.ctti_size == 0)
    {
      /* This is a (redundant) definition of void.  */
      voids.safe_push (ctftype->dtd_type);
      holes.safe_push (ctftype->dtd_type);
    }

  arg_ctfc->ctfc_types_list[index] = ctftype;

  return 1;
}

/* Preprocessing callback routine for variables.  */

int
btf_dvd_emit_preprocess_cb (ctf_dvdef_ref *slot, ctf_container_ref arg_ctfc)
{
  ctf_dvdef_ref var = (ctf_dvdef_ref) * slot;

  /* If this is an extern variable declaration with a defining declaration
     later, skip it so that only the defining declaration is emitted.
     This is the same case, fix and reasoning as in CTF; see PR105089.  */
  if (ctf_dvd_ignore_lookup (arg_ctfc, var->dvd_key))
    return 1;

  /* Do not add variables which refer to unsupported types.  */
  if (!voids.contains (var->dvd_type) && btf_removed_type_p (var->dvd_type))
    return 1;

  arg_ctfc->ctfc_vars_list[num_vars_added] = var;
  btf_var_ids->put (var, num_vars_added);

  num_vars_added++;
  num_types_created++;

  return 1;
}

/* Preprocessing callback routine for types.  */

static void
btf_dtd_emit_preprocess_cb (ctf_container_ref ctfc, ctf_dtdef_ref dtd)
{
  if (!btf_emit_id_p (dtd->dtd_type))
    return;

  ctfc->ctfc_num_vlen_bytes += btf_calc_num_vbytes (dtd);
}

/* Preprocess the CTF information to prepare for BTF output.  BTF is almost a
   subset of CTF, with many small differences in encoding, and lacking support
   for some types (notably floating point formats).

   During the preprocessing pass:
   - Ascertain that the sorted list of types has been prepared.  For the BTF
     generation process, this is taken care of by the btf_init_postprocess ().

   - BTF_KIND_FUNC and BTF_KIND_DATASEC records are constructed. These types do
     not have analogues in CTF (the analogous type to CTF_K_FUNCTION is
     BTF_KIND_FUNC_PROTO), but can be relatively easily deduced from CTF
     information.

   - Construct BTF_KIND_VAR records, representing variables.

   - Calculate the total size in bytes of variable-length information following
     BTF type records. This is used for outputting the BTF header.

   After preprocessing, all BTF information is ready to be output:
   - ctfc->ctfc_types_list holdstypes converted from CTF types. This does not
     include KIND_VAR, KIND_FUNC, nor KIND_DATASEC types. These types have been
     re-encoded to the appropriate representation in BTF.
   - ctfc->ctfc_vars_list holds all variables which should be output.
     Variables of unsupported types are not present in this list.
   - Vector 'funcs' holds all BTF_KIND_FUNC types, one to match each
     BTF_KIND_FUNC_PROTO.
   - Vector 'datasecs' holds all BTF_KIND_DATASEC types.  */

static void
btf_emit_preprocess (ctf_container_ref ctfc)
{
  size_t num_ctf_types = ctfc->ctfc_types->elements ();
  size_t num_ctf_vars = ctfc->ctfc_vars->elements ();
  size_t i;

  if (num_ctf_types)
    {
      gcc_assert (ctfc->ctfc_types_list);
      /* Preprocess the types.  */
      for (i = 1; i <= num_ctf_types; i++)
	btf_dtd_emit_preprocess_cb (ctfc, ctfc->ctfc_types_list[i]);
    }

  btf_var_ids = hash_map<ctf_dvdef_ref, unsigned int>::create_ggc (100);

  if (num_ctf_vars)
    {
      /* Allocate and construct the list of variables. While BTF variables are
	 not distinct from types (in that variables are simply types with
	 BTF_KIND_VAR), it is simpler to maintain a separate list of variables
	 and append them to the types list during output.  */
      ctfc->ctfc_vars_list = ggc_vec_alloc<ctf_dvdef_ref>(num_ctf_vars);
      ctfc->ctfc_vars->traverse<ctf_container_ref, btf_dvd_emit_preprocess_cb>
	(ctfc);

      ctfc->ctfc_num_vlen_bytes += (num_vars_added * sizeof (struct btf_var));
    }

  btf_collect_datasec (ctfc);
}

/* Return true iff DMD is a member description of a bit-field which can be
   validly represented in BTF.  */

static bool
btf_dmd_representable_bitfield_p (ctf_container_ref ctfc, ctf_dmdef_t *dmd)
{
  ctf_dtdef_ref ref_type = ctfc->ctfc_types_list[dmd->dmd_type];

  if (CTF_V2_INFO_KIND (ref_type->dtd_data.ctti_info) == CTF_K_SLICE)
    {
      unsigned short word_offset = ref_type->dtd_u.dtu_slice.cts_offset;
      unsigned short bits = ref_type->dtd_u.dtu_slice.cts_bits;
      uint64_t sou_offset = dmd->dmd_offset;

      if ((bits > 0xff) || ((sou_offset + word_offset) > 0xffffff))
	return false;

      return true;
    }

  return false;
}

/* BTF asm helper routines.  */

/* Asm'out a BTF type. This routine is responsible for the bulk of the task
   of converting CTF types to their BTF representation.  */

static void
btf_asm_type (ctf_container_ref ctfc, ctf_dtdef_ref dtd)
{
  uint32_t btf_kind, btf_kflag, btf_vlen, btf_size_type;
  uint32_t ctf_info = dtd->dtd_data.ctti_info;

  btf_kind = get_btf_kind (CTF_V2_INFO_KIND (ctf_info));
  btf_size_type = dtd->dtd_data.ctti_type;
  btf_vlen = CTF_V2_INFO_VLEN (ctf_info);

  /* By now any unrepresentable types have been removed.  */
  gcc_assert (btf_kind != BTF_KIND_UNKN);

  /* Size 0 integers are redundant definitions of void. None should remain
     in the types list by this point.  */
  gcc_assert (btf_kind != BTF_KIND_INT || btf_size_type >= 1);

  /* Re-encode the ctti_info to BTF.  */
  /* kflag is 1 for structs/unions with a bitfield member.
     kflag is 1 for forwards to unions.
     kflag is 0 in all other cases.  */
  btf_kflag = 0;

  if (btf_kind == BTF_KIND_STRUCT || btf_kind == BTF_KIND_UNION)
    {
      /* If a struct/union has ANY bitfield members, set kflag=1.
	 Note that we must also change the encoding of every member to encode
	 both member bitfield size (stealing most-significant 8 bits) and bit
	 offset (LS 24 bits). This is done during preprocessing.  */
      ctf_dmdef_t *dmd;
      for (dmd = dtd->dtd_u.dtu_members;
	   dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
	{
	  /* Set kflag if this member is a representable bitfield.  */
	  if (btf_dmd_representable_bitfield_p (ctfc, dmd))
	    btf_kflag = 1;

	  /* Struct members that refer to unsupported types or bitfield formats
	     shall be skipped. These are marked during preprocessing.  */
	  else if (!btf_emit_id_p (dmd->dmd_type))
	    btf_vlen -= 1;
	}
    }

  /* BTF forwards make use of KIND_FLAG to distinguish between forwards to
     structs and forwards to unions. The dwarf2ctf conversion process stores
     the kind of the forward in ctti_type, but for BTF this must be 0 for
     forwards, with only the KIND_FLAG to distinguish.
     At time of writing, BTF forwards to enums are unspecified.  */
  if (btf_kind == BTF_KIND_FWD)
    {
      if (dtd->dtd_data.ctti_type == CTF_K_UNION)
	btf_kflag = 1;

      btf_size_type = 0;
    }

  if (btf_kind == BTF_KIND_ENUM)
    {
      btf_kflag = dtd->dtd_enum_unsigned
		    ? BTF_KF_ENUM_UNSIGNED
		    : BTF_KF_ENUM_SIGNED;
      if (dtd->dtd_data.ctti_size == 0x8)
	btf_kind = BTF_KIND_ENUM64;
   }

  dw2_asm_output_data (4, dtd->dtd_data.ctti_name, "btt_name");
  dw2_asm_output_data (4, BTF_TYPE_INFO (btf_kind, btf_kflag, btf_vlen),
		       "btt_info: kind=%u, kflag=%u, vlen=%u",
		       btf_kind, btf_kflag, btf_vlen);
  switch (btf_kind)
    {
    case BTF_KIND_INT:
    case BTF_KIND_FLOAT:
    case BTF_KIND_STRUCT:
    case BTF_KIND_UNION:
    case BTF_KIND_ENUM:
    case BTF_KIND_DATASEC:
    case BTF_KIND_ENUM64:
      dw2_asm_output_data (4, dtd->dtd_data.ctti_size, "btt_size: %uB",
			   dtd->dtd_data.ctti_size);
      return;
    default:
      break;
    }

  dw2_asm_output_data (4, get_btf_id (dtd->dtd_data.ctti_type), "btt_type");
}

/* Asm'out the variable information following a BTF_KIND_ARRAY.  */

static void
btf_asm_array (ctf_dtdef_ref dtd)
{
  dw2_asm_output_data (4, get_btf_id (dtd->dtd_u.dtu_arr.ctr_contents),
		       "bta_contents");
  dw2_asm_output_data (4, get_btf_id (dtd->dtd_u.dtu_arr.ctr_index),
		       "bta_index");
  dw2_asm_output_data (4, dtd->dtd_u.dtu_arr.ctr_nelems, "bta_nelems");
}

/* Asm'out a BTF_KIND_VAR.  */

static void
btf_asm_varent (ctf_dvdef_ref var)
{
  dw2_asm_output_data (4, var->dvd_name_offset, "btv_name");
  dw2_asm_output_data (4, BTF_TYPE_INFO (BTF_KIND_VAR, 0, 0), "btv_info");
  dw2_asm_output_data (4, get_btf_id (var->dvd_type), "btv_type");
  dw2_asm_output_data (4, var->dvd_visibility, "btv_linkage");
}

/* Asm'out a member description following a BTF_KIND_STRUCT or
   BTF_KIND_UNION.  */

static void
btf_asm_sou_member (ctf_container_ref ctfc, ctf_dmdef_t * dmd)
{
  ctf_dtdef_ref ref_type = ctfc->ctfc_types_list[dmd->dmd_type];

  /* Re-encode bitfields to BTF representation.  */
  if (CTF_V2_INFO_KIND (ref_type->dtd_data.ctti_info) == CTF_K_SLICE)
    {
      ctf_id_t base_type = ref_type->dtd_u.dtu_slice.cts_type;
      unsigned short word_offset = ref_type->dtd_u.dtu_slice.cts_offset;
      unsigned short bits = ref_type->dtd_u.dtu_slice.cts_bits;
      uint64_t sou_offset = dmd->dmd_offset;

      /* Pack the bit offset and bitfield size together.  */
      sou_offset += word_offset;

      /* If this bitfield cannot be represented, do not output anything.
	 The parent struct/union 'vlen' field has already been updated.  */
      if ((bits > 0xff) || (sou_offset > 0xffffff))
	return;

      sou_offset &= 0x00ffffff;
      sou_offset |= ((bits & 0xff) << 24);

      /* Refer to the base type of the slice.  */
      dw2_asm_output_data (4, dmd->dmd_name_offset, "btm_name_off");
      dw2_asm_output_data (4, get_btf_id (base_type), "btm_type");
      dw2_asm_output_data (4, sou_offset, "btm_offset");
    }
  else
    {
      dw2_asm_output_data (4, dmd->dmd_name_offset, "btm_name_off");
      dw2_asm_output_data (4, get_btf_id (dmd->dmd_type), "btm_type");
      dw2_asm_output_data (4, dmd->dmd_offset, "btm_offset");
    }
}

/* Asm'out an enum constant following a BTF_KIND_ENUM{,64}.  */

static void
btf_asm_enum_const (unsigned int size, ctf_dmdef_t * dmd)
{
  dw2_asm_output_data (4, dmd->dmd_name_offset, "bte_name");
  if (size == 4)
    dw2_asm_output_data (size, dmd->dmd_value, "bte_value");
  else
    {
      dw2_asm_output_data (4, dmd->dmd_value & 0xffffffff, "bte_value_lo32");
      dw2_asm_output_data (4, (dmd->dmd_value >> 32) & 0xffffffff, "bte_value_hi32");
    }
}

/* Asm'out a function parameter description following a BTF_KIND_FUNC_PROTO.  */

static void
btf_asm_func_arg (ctf_func_arg_t * farg, size_t stroffset)
{
  /* If the function arg does not have a name, refer to the null string at
     the start of the string table. This ensures correct encoding for varargs
     '...' arguments.  */
  if ((farg->farg_name != NULL) && strcmp (farg->farg_name, ""))
    dw2_asm_output_data (4, farg->farg_name_offset + stroffset, "farg_name");
  else
    dw2_asm_output_data (4, 0, "farg_name");

  dw2_asm_output_data (4, (btf_removed_type_p (farg->farg_type)
			   ? BTF_VOID_TYPEID
			   : get_btf_id (farg->farg_type)),
		       "farg_type");
}

/* Asm'out a BTF_KIND_FUNC type.  */

static void
btf_asm_func_type (ctf_dtdef_ref dtd)
{
  dw2_asm_output_data (4, dtd->dtd_data.ctti_name, "btt_name");
  dw2_asm_output_data (4, BTF_TYPE_INFO (BTF_KIND_FUNC, 0,
                                         dtd->linkage),
                       "btt_info: kind=%u, kflag=%u, linkage=%u",
                       BTF_KIND_FUNC, 0, dtd->linkage);
  dw2_asm_output_data (4, get_btf_id (dtd->dtd_data.ctti_type), "btt_type");
}

/* Asm'out a variable entry following a BTF_KIND_DATASEC.  */

static void
btf_asm_datasec_entry (struct btf_var_secinfo info)
{
  dw2_asm_output_data (4, info.type, "bts_type");
  dw2_asm_output_data (4, info.offset, "bts_offset");
  dw2_asm_output_data (4, info.size, "bts_size");
}

/* Asm'out a whole BTF_KIND_DATASEC, including its variable entries.  */

static void
btf_asm_datasec_type (btf_datasec_t ds, size_t stroffset)
{
  dw2_asm_output_data (4, ds.name_offset + stroffset, "btt_name");
  dw2_asm_output_data (4, BTF_TYPE_INFO (BTF_KIND_DATASEC, 0,
					 ds.entries.length ()),
		       "btt_info");
  /* Note: the "total section size in bytes" is emitted as 0 and patched by
     loaders such as libbpf.  */
  dw2_asm_output_data (4, 0, "btt_size");
  for (size_t i = 0; i < ds.entries.length (); i++)
    btf_asm_datasec_entry (ds.entries[i]);
}

/* Compute and output the header information for a .BTF section.  */

static void
output_btf_header (ctf_container_ref ctfc)
{
   switch_to_section (btf_info_section);
   ASM_OUTPUT_LABEL (asm_out_file, btf_info_section_label);

   /* BTF magic number, version, flags, and header length.  */
   dw2_asm_output_data (2, BTF_MAGIC, "btf_magic");
   dw2_asm_output_data (1, BTF_VERSION, "btf_version");
   dw2_asm_output_data (1, 0, "btf_flags");
   dw2_asm_output_data (4, sizeof (struct btf_header), "btf_hdr_len");

   uint32_t type_off = 0, type_len = 0;
   uint32_t str_off = 0, str_len = 0;
   uint32_t datasec_vlen_bytes = 0;

   if (!ctfc_is_empty_container (ctfc))
     {
       for (size_t i = 0; i < datasecs.length (); i++)
	 {
	   datasec_vlen_bytes += ((datasecs[i].entries.length ())
				  * sizeof (struct btf_var_secinfo));
	 }

       /* Total length (bytes) of the types section.  */
       type_len = (num_types_added * sizeof (struct btf_type))
	 + (num_types_created * sizeof (struct btf_type))
	 + datasec_vlen_bytes
	 + ctfc->ctfc_num_vlen_bytes;

       str_off = type_off + type_len;

       str_len = ctfc->ctfc_strtable.ctstab_len
	 + ctfc->ctfc_aux_strtable.ctstab_len;
     }

   /* Offset of type section.  */
   dw2_asm_output_data (4, type_off, "type_off");
   /* Length of type section in bytes.  */
   dw2_asm_output_data (4, type_len, "type_len");
    /* Offset of string section.  */
   dw2_asm_output_data (4, str_off, "str_off");
    /* Length of string section in bytes.  */
   dw2_asm_output_data (4, str_len, "str_len");
}

/* Output all BTF_KIND_VARs in CTFC.  */

static void
output_btf_vars (ctf_container_ref ctfc)
{
  size_t i;
  size_t num_ctf_vars = num_vars_added;
  if (num_ctf_vars)
    {
      for (i = 0; i < num_ctf_vars; i++)
	btf_asm_varent (ctfc->ctfc_vars_list[i]);
    }
}

/* Output BTF string records. The BTF strings section is a concatenation
   of the standard and auxilliary string tables in the ctf container.  */

static void
output_btf_strs (ctf_container_ref ctfc)
{
  ctf_string_t * ctf_string = ctfc->ctfc_strtable.ctstab_head;

  while (ctf_string)
    {
      dw2_asm_output_nstring (ctf_string->cts_str, -1, "btf_string");
      ctf_string = ctf_string->cts_next;
    }

  ctf_string = ctfc->ctfc_aux_strtable.ctstab_head;
  while (ctf_string)
    {
      dw2_asm_output_nstring (ctf_string->cts_str, -1, "btf_aux_string");
      ctf_string = ctf_string->cts_next;
    }
}

/* Output all (representable) members of a BTF_KIND_STRUCT or
   BTF_KIND_UNION type.  */

static void
output_asm_btf_sou_fields (ctf_container_ref ctfc, ctf_dtdef_ref dtd)
{
  ctf_dmdef_t * dmd;

  for (dmd = dtd->dtd_u.dtu_members;
       dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
      btf_asm_sou_member (ctfc, dmd);
}

/* Output all enumerator constants following a BTF_KIND_ENUM{,64}.  */

static void
output_asm_btf_enum_list (ctf_container_ref ARG_UNUSED (ctfc),
			  ctf_dtdef_ref dtd)
{
  ctf_dmdef_t * dmd;

  for (dmd = dtd->dtd_u.dtu_members;
       dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
    btf_asm_enum_const (dtd->dtd_data.ctti_size, dmd);
}

/* Output all function arguments following a BTF_KIND_FUNC_PROTO.  */

static void
output_asm_btf_func_args_list (ctf_container_ref ctfc,
			       ctf_dtdef_ref dtd)
{
  size_t farg_name_offset = ctfc_get_strtab_len (ctfc, CTF_STRTAB);
  ctf_func_arg_t * farg;
  for (farg = dtd->dtd_u.dtu_argv;
       farg != NULL; farg = (ctf_func_arg_t *) ctf_farg_list_next (farg))
    btf_asm_func_arg (farg, farg_name_offset);
}

/* Output the variable portion of a BTF type record. The information depends
   on the kind of the type.  */

static void
output_asm_btf_vlen_bytes (ctf_container_ref ctfc, ctf_dtdef_ref dtd)
{
  uint32_t btf_kind, encoding;

  btf_kind = get_btf_kind (CTF_V2_INFO_KIND (dtd->dtd_data.ctti_info));

  if (btf_kind == BTF_KIND_UNKN)
    return;

  switch (btf_kind)
    {
    case BTF_KIND_INT:
      /* Redundant definitions of void may still be hanging around in the type
	 list as size 0 integers. Skip emitting them.  */
      if (dtd->dtd_data.ctti_size < 1)
	break;

      /* In BTF the CHAR `encoding' seems to not be used, so clear it
         here.  */
      dtd->dtd_u.dtu_enc.cte_format &= ~BTF_INT_CHAR;

      encoding = BTF_INT_DATA (dtd->dtd_u.dtu_enc.cte_format,
			       dtd->dtd_u.dtu_enc.cte_offset,
			       dtd->dtd_u.dtu_enc.cte_bits);

      dw2_asm_output_data (4, encoding, "bti_encoding");
      break;

    case BTF_KIND_ARRAY:
      btf_asm_array (dtd);
      break;

    case BTF_KIND_STRUCT:
    case BTF_KIND_UNION:
      output_asm_btf_sou_fields (ctfc, dtd);
      break;

    case BTF_KIND_ENUM:
      output_asm_btf_enum_list (ctfc, dtd);
      break;

    case BTF_KIND_FUNC_PROTO:
      output_asm_btf_func_args_list (ctfc, dtd);
      break;

    case BTF_KIND_VAR:
      /* BTF Variables are handled by output_btf_vars and btf_asm_varent.
	 There should be no BTF_KIND_VAR types at this point.  */
      gcc_unreachable ();

    case BTF_KIND_DATASEC:
      /* The BTF_KIND_DATASEC records are handled by output_btf_datasec_types
	 and btf_asm_datasec_type. There should be no BTF_KIND_DATASEC types
	 at this point.  */
      gcc_unreachable ();

    default:
      /* All other BTF type kinds have no variable length data.  */
      break;
    }
}

/* Output a whole BTF type record for TYPE, including the fixed and variable
   data portions.  */

static void
output_asm_btf_type (ctf_container_ref ctfc, ctf_dtdef_ref type)
{
  if (btf_emit_id_p (type->dtd_type))
    {
      btf_asm_type (ctfc, type);
      output_asm_btf_vlen_bytes (ctfc, type);
    }
}

/* Output all BTF types in the container. This does not include synthesized
   types: BTF_KIND_VAR, BTF_KIND_FUNC, nor BTF_KIND_DATASEC.  */

static void
output_btf_types (ctf_container_ref ctfc)
{
  size_t i;
  size_t num_types = ctfc->ctfc_types->elements ();
  if (num_types)
    {
      for (i = 1; i <= num_types; i++)
	output_asm_btf_type (ctfc, ctfc->ctfc_types_list[i]);
    }
}

/* Output all BTF_KIND_FUNC type records.  */

static void
output_btf_func_types (void)
{
  for (size_t i = 0; i < vec_safe_length (funcs); i++)
    btf_asm_func_type ((*funcs)[i]);
}

/* Output all BTF_KIND_DATASEC records.  */

static void
output_btf_datasec_types (ctf_container_ref ctfc)
{
  size_t name_offset = ctfc_get_strtab_len (ctfc, CTF_STRTAB);

  for (size_t i = 0; i < datasecs.length(); i++)
    btf_asm_datasec_type (datasecs[i], name_offset);
}

/* Postprocess the CTF debug data post initialization.

   During the postprocess pass:

   - Prepare the sorted list of BTF types.

     The sorted list of BTF types is, firstly, used for lookup (during the BTF
     generation process) of CTF/BTF types given a typeID.

     Secondly, in the emitted BTF section, BTF Types need to be in the sorted
     order of their type IDs.  The BTF types section is viewed as an array,
     with type IDs used to index into that array.  It is essential that every
     type be placed at the exact index corresponding to its ID, or else
     references to that type from other types will no longer be correct.

   - References to void types are converted to reference BTF_VOID_TYPEID. In
     CTF, a distinct type is used to encode void.

   - Bitfield struct/union members are converted to BTF encoding. CTF uses
     slices to encode bitfields, but BTF does not have slices and encodes
     bitfield information directly in the variable-length btf_member
     descriptions following the struct or union type.

   - Unrepresentable types are removed. We cannot have any invalid BTF types
     appearing in the output so they must be removed, and type ids of other
     types and references adjust accordingly. This also involves ensuring that
     BTF descriptions of struct members referring to unrepresentable types are
     not emitted, as they would be nonsensical.

   - Adjust inner- and inter-type references-by-ID to account for removed
     types, and construct the types list.  */

void
btf_init_postprocess (void)
{
  ctf_container_ref tu_ctfc = ctf_get_tu_ctfc ();

  holes.create (0);
  voids.create (0);

  num_types_added = 0;
  num_types_created = 0;

  /* Workaround for 'const void' variables.  These variables are sometimes used
     in eBPF programs to address kernel symbols.  DWARF does not generate const
     qualifier on void type, so we would incorrectly emit these variables
     without the const qualifier.
     Unfortunately we need the TREE node to know it was const, and we need
     to create the const modifier type (if needed) now, before making the types
     list.  So we can't avoid iterating with FOR_EACH_VARIABLE here, and then
     again when creating the DATASEC entries.  */
  ctf_id_t constvoid_id = CTF_NULL_TYPEID;
  varpool_node *var;
  FOR_EACH_VARIABLE (var)
    {
      if (!var->decl)
	continue;

      tree type = TREE_TYPE (var->decl);
      if (type && VOID_TYPE_P (type) && TYPE_READONLY (type))
	{
	  dw_die_ref die = lookup_decl_die (var->decl);
	  if (die == NULL)
	    continue;

	  ctf_dvdef_ref dvd = ctf_dvd_lookup (tu_ctfc, die);
	  if (dvd == NULL)
	    continue;

	  /* Create the 'const' modifier type for void.  */
	  if (constvoid_id == CTF_NULL_TYPEID)
	    constvoid_id = ctf_add_reftype (tu_ctfc, CTF_ADD_ROOT,
					    dvd->dvd_type, CTF_K_CONST, NULL);
	  dvd->dvd_type = constvoid_id;
	}
    }

  size_t i;
  size_t num_ctf_types = tu_ctfc->ctfc_types->elements ();

  if (num_ctf_types)
    {
      init_btf_id_map (num_ctf_types + 1);

      /* Allocate the types list and traverse all types, placing each type
	 at the index according to its ID.  Add 1 because type ID 0 always
	 represents VOID.  */
      tu_ctfc->ctfc_types_list
	= ggc_vec_alloc<ctf_dtdef_ref>(num_ctf_types + 1);
      tu_ctfc->ctfc_types->traverse<ctf_container_ref, btf_dtd_postprocess_cb>
	(tu_ctfc);

      /* Build mapping of CTF type ID -> BTF type ID, and count total number
	 of valid BTF types added.  */
      for (i = 1; i <= num_ctf_types; i++)
	{
	  ctf_dtdef_ref dtd = tu_ctfc->ctfc_types_list[i];
	  ctf_id_t btfid = btf_adjust_type_id (dtd->dtd_type);
	  set_btf_id (dtd->dtd_type, btfid);
	  if (btfid < BTF_MAX_TYPE && (btfid != BTF_VOID_TYPEID))
	    num_types_added ++;
	}
    }
}

/* Process and output all BTF data. Entry point of btfout.  */

void
btf_output (const char * filename)
{
  ctf_container_ref tu_ctfc = ctf_get_tu_ctfc ();

  init_btf_sections ();

  datasecs.create (0);
  vec_alloc (funcs, 16);

  ctf_add_cuname (tu_ctfc, filename);

  btf_emit_preprocess (tu_ctfc);

  output_btf_header (tu_ctfc);
  output_btf_types (tu_ctfc);
  output_btf_vars (tu_ctfc);
  output_btf_func_types ();
  output_btf_datasec_types (tu_ctfc);
  output_btf_strs (tu_ctfc);
}

/* Reset all state for BTF generation so that we can rerun the compiler within
   the same process.  */

void
btf_finalize (void)
{
  btf_info_section = NULL;

  /* Clear preprocessing state.  */
  num_vars_added = 0;
  num_types_added = 0;
  num_types_created = 0;

  holes.release ();
  voids.release ();
  for (size_t i = 0; i < datasecs.length (); i++)
    datasecs[i].entries.release ();
  datasecs.release ();

  funcs = NULL;

  btf_var_ids->empty ();
  btf_var_ids = NULL;

  free (btf_id_map);
  btf_id_map = NULL;

  ctf_container_ref tu_ctfc = ctf_get_tu_ctfc ();
  ctfc_delete_container (tu_ctfc);
  tu_ctfc = NULL;
}

#include "gt-btfout.h"
