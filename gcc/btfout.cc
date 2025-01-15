/* Output BTF format from GCC.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.

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
#include "stringpool.h"  /* For lookup_attribute.  */
#include "attribs.h" /* For lookup_attribute.  */
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

#define BTF_INVALID_TYPEID 0xFFFFFFFF

/* Internal representation of an entry in a BTF_KIND_DATASEC record.  */
struct btf_datasec_entry
{
  union {
    ctf_dvdef_ref dvd; /* Reference to the underlying variable represented.  */
    ctf_dtdef_ref dtd; /* Reference to the underlying type represented.  */
  };
  bool is_var;	       /* True iff this entry represents a variable.  */
  uint32_t size;       /* Size of variable or function, in bytes.
			  For functions, always zero at compile time.  */
};

/* Internal representation of a BTF_KIND_DATASEC record.  */
typedef struct btf_datasec
{
  ctf_id_t id;				 /* BTF type ID of this record.  */
  const char *name;			 /* Section name, e.g. ".bss".  */
  uint32_t name_offset;			 /* Offset to name in string table.  */
  vec<struct btf_datasec_entry> entries; /* Entries in this section.  */
} btf_datasec_t;

/* One BTF_KIND_DATASEC record is created for each output data section which
   will hold at least one variable.  */
static vec<btf_datasec_t> datasecs;

/* Functions in BTF have two separate type records - one for the prototype
   (BTF_KIND_FUNC_PROTO), as well as a BTF_KIND_FUNC. CTF_K_FUNCTION types
   map closely to BTF_KIND_FUNC_PROTO, but the BTF_KIND_FUNC records must be
   created. This vector holds them.  */
static GTY (()) vec<ctf_dtdef_ref, va_gc> *funcs;

/* Maps BTF_KIND_FUNC_PROTO to the BTF_KIND_FUNC record for it.  Used when
   creating DATASEC entries.  */
static GTY (()) hash_map<ctf_dtdef_ref, ctf_dtdef_ref> *func_map;

/* Highest BTF ID assigned to any regular type translated from CTF.
   Does not include BTF_KIND_{VAR,FUNC,DATASEC} types.  */
static ctf_id_t max_translated_id = 0;

/* Name strings for BTF kinds.
   Note: the indices here must match the type defines in btf.h.  */
static const char *const btf_kind_names[] =
  {
    "UNKN", "INT", "PTR", "ARRAY", "STRUCT", "UNION", "ENUM", "FWD",
    "TYPEDEF", "VOLATILE", "CONST", "RESTRICT", "FUNC", "FUNC_PROTO",
    "VAR", "DATASEC", "FLOAT", "DECL_TAG", "TYPE_TAG", "ENUM64"
  };

/* Return a name string for the given BTF_KIND.  */

static const char *
btf_kind_name (uint32_t btf_kind)
{
  return btf_kind_names[btf_kind];
}

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

/* Convenience wrapper around get_btf_kind for the common case.  */

uint32_t
btf_dtd_kind (ctf_dtdef_ref dtd)
{
  if (!dtd)
    return BTF_KIND_UNKN;
  return get_btf_kind (CTF_V2_INFO_KIND (dtd->dtd_data.ctti_info));
}

/* Some BTF types, like BTF_KIND_FUNC_PROTO, are anonymous.  The machinery
   in btfout to emit BTF, may reset dtd_data->ctti_name, but does not update
   the name in the ctf_dtdef_ref type object (deliberate choice).  This
   interface helps abstract out that state of affairs, while giving access to
   the name of the type as intended.  */

static const char *
get_btf_type_name (ctf_dtdef_ref dtd)
{
  const char *anon = "";
  return (dtd->dtd_data.ctti_name) ? dtd->dtd_name : anon;
}

static bool
btf_emit_type_p (ctf_dtdef_ref dtd)
{
  uint32_t kind = btf_dtd_kind (dtd);

  if (kind == BTF_KIND_UNKN)
    /* This type is not representable in BTF.  */
    return false;

  if (kind == BTF_KIND_INT && dtd->dtd_data.ctti_size == 0)
    /* This is a (redundant) definition of void.  */
    return false;

  return true;
}

/* Return true if DTD is a forward-declared enum.  The BTF representation
   of forward declared enums is not formally defined.  */

static bool
btf_fwd_to_enum_p (ctf_dtdef_ref dtd)
{
  uint32_t kind = btf_dtd_kind (dtd);
  return (kind == BTF_KIND_FWD && dtd->dtd_data.ctti_type == CTF_K_ENUM);
}

/* Each BTF type can be followed additional, variable-length information
   completing the description of the type. Calculate the number of bytes
   of variable information required to encode a given type.  */

static uint64_t
btf_calc_num_vbytes (ctf_dtdef_ref dtd)
{
  uint64_t vlen_bytes = 0;

  uint32_t kind = btf_dtd_kind (dtd);
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
      vlen_bytes += (dtd->dtd_data.ctti_size > 4)
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

/* Return true iff DMD is a member description of a bit-field which can be
   validly represented in BTF.  */

static bool
btf_dmd_representable_bitfield_p (ctf_dmdef_t *dmd)
{
  ctf_dtdef_ref ref_type = dmd->dmd_type;
  if (!ref_type)
    return false;

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

/* Asm'out a reference to another BTF type.  */

static void
btf_asm_type_ref (const char *prefix, ctf_dtdef_ref dtd)
{
  if (!dtd || !btf_emit_type_p (dtd))
    dw2_asm_output_data (4, BTF_VOID_TYPEID, "%s: void", prefix);
  else
    {
      uint32_t kind = btf_dtd_kind (dtd);
      if (btf_fwd_to_enum_p (dtd))
	kind = BTF_KIND_ENUM;
      else if (kind == BTF_KIND_FUNC_PROTO && dtd->dtd_type > max_translated_id)
	kind = BTF_KIND_FUNC;

      dw2_asm_output_data (4, dtd->dtd_type, "%s: (BTF_KIND_%s '%s')",
			   prefix, btf_kind_name (kind),
			   get_btf_type_name (dtd));
    }
}

/* Asm'out a BTF type. This routine is responsible for the bulk of the task
   of converting CTF types to their BTF representation.  */

static void
btf_asm_type (ctf_dtdef_ref dtd)
{
  uint32_t btf_kind, btf_kflag, btf_vlen, btf_size;
  uint32_t ctf_info = dtd->dtd_data.ctti_info;

  btf_kind = btf_dtd_kind (dtd);
  btf_size = dtd->dtd_data.ctti_size;
  btf_vlen = CTF_V2_INFO_VLEN (ctf_info);

  /* By now any unrepresentable types have been removed.  */
  gcc_assert (btf_kind != BTF_KIND_UNKN);

  /* Size 0 integers are redundant definitions of void. None should remain
     in the types list by this point.  */
  gcc_assert (btf_kind != BTF_KIND_INT || btf_size >= 1);

  /* Re-encode the ctti_info to BTF.  */
  /* kflag is 1 for structs/unions with a bitfield member.
     kflag is 1 for forwards to unions.
     kflag is 0 in all other cases.  */
  btf_kflag = 0;

  if (btf_kind == BTF_KIND_STRUCT || btf_kind == BTF_KIND_UNION)
    {
      /* If a struct/union has ANY bitfield members, set kflag=1.  */
      ctf_dmdef_t *dmd;
      for (dmd = dtd->dtd_u.dtu_members;
	   dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
	{
	  /* Set kflag if this member is a representable bitfield.  */
	  if (btf_dmd_representable_bitfield_p (dmd))
	    {
	      btf_kflag = 1;
	      break;
	    }
	}
    }

  /* BTF forwards make use of KIND_FLAG to distinguish between forwards to
     structs and forwards to unions. The dwarf2ctf conversion process stores
     the kind of the forward in ctti_type, but for BTF this must be 0 for
     forwards, with only the KIND_FLAG to distinguish.
     Forwards to enum types are special-cased below.  */
  else if (btf_kind == BTF_KIND_FWD)
    {
      if (dtd->dtd_data.ctti_type == CTF_K_UNION)
	btf_kflag = 1;

      /* PR debug/111735.  Encode foward-declared enums as BTF_KIND_ENUM
	 with vlen=0.  A representation for these is not formally defined;
	 this is the de-facto standard used by other tools like clang
	 and pahole.  */
      else if (dtd->dtd_data.ctti_type == CTF_K_ENUM)
	{
	  btf_kind = BTF_KIND_ENUM;
	  btf_vlen = 0;
	}

      btf_size = 0;
    }

  else if (btf_kind == BTF_KIND_ENUM)
    {
      btf_kflag = dtd->dtd_enum_unsigned
		    ? BTF_KF_ENUM_UNSIGNED
		    : BTF_KF_ENUM_SIGNED;
      if (dtd->dtd_data.ctti_size == 0x8)
	btf_kind = BTF_KIND_ENUM64;
    }

  /* PR debug/112656.  BTF_KIND_FUNC_PROTO is always anonymous.  */
  else if (btf_kind == BTF_KIND_FUNC_PROTO)
    dtd->dtd_data.ctti_name = 0;

  dw2_asm_output_data (4, dtd->dtd_data.ctti_name,
		       "TYPE %" PRIu64 " BTF_KIND_%s '%s'",
		       dtd->dtd_type, btf_kind_name (btf_kind),
		       get_btf_type_name (dtd));
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
      dw2_asm_output_data (4, btf_size, "btt_size: %uB", btf_size);
      return;
    case BTF_KIND_ARRAY:
    case BTF_KIND_FWD:
      /* These types do not encode any information in the size/type field
	 and should write 0.  */
      dw2_asm_output_data (4, 0, "(unused)");
      return;
    default:
      break;
    }

  btf_asm_type_ref ("btt_type", dtd->ref_type);
}

/* Asm'out the variable information following a BTF_KIND_ARRAY.  */

static void
btf_asm_array (ctf_arinfo_t arr)
{
  btf_asm_type_ref ("bta_elem_type", arr.ctr_contents);
  btf_asm_type_ref ("bta_index_type", arr.ctr_index);
  dw2_asm_output_data (4, arr.ctr_nelems, "bta_nelems");
}

/* Asm'out a BTF_KIND_VAR.  */

static void
btf_asm_varent (ctf_dvdef_ref var)
{
  dw2_asm_output_data (4, var->dvd_name_offset,
		       "TYPE %" PRIu64 " BTF_KIND_VAR '%s'",
		       var->dvd_id, var->dvd_name);
  dw2_asm_output_data (4, BTF_TYPE_INFO (BTF_KIND_VAR, 0, 0), "btv_info");
  btf_asm_type_ref ("btv_type", var->dvd_type);
  dw2_asm_output_data (4, var->dvd_visibility, "btv_linkage");
}

/* Asm'out a member description following a BTF_KIND_STRUCT or
   BTF_KIND_UNION.  */

static void
btf_asm_sou_member (ctf_dmdef_t * dmd, unsigned int idx)
{
  ctf_dtdef_ref base_type = dmd->dmd_type;
  uint64_t sou_offset = dmd->dmd_offset;

  dw2_asm_output_data (4, dmd->dmd_name_offset,
		       "MEMBER '%s' idx=%u",
		       dmd->dmd_name, idx);

  if (base_type
      && CTF_V2_INFO_KIND (base_type->dtd_data.ctti_info) == CTF_K_SLICE)
    {
      if (btf_dmd_representable_bitfield_p (dmd))
	{
	  unsigned short word_offset = base_type->dtd_u.dtu_slice.cts_offset;
	  unsigned short bits = base_type->dtd_u.dtu_slice.cts_bits;

	  /* Pack the bit offset and bitfield size together.  */
	  sou_offset += word_offset;
	  sou_offset &= 0x00ffffff;
	  sou_offset |= ((bits & 0xff) << 24);

	  /* Refer to the base type of the slice.  */
	  base_type = base_type->dtd_u.dtu_slice.cts_type;
	}
      else
	{
	  /* Bitfield cannot be represented in BTF.  Emit the member as having
	     'void' type.  */
	  base_type = NULL;
	}
    }

  btf_asm_type_ref ("btm_type", base_type);
  dw2_asm_output_data (4, sou_offset, "btm_offset");
}

/* Asm'out an enum constant following a BTF_KIND_ENUM{,64}.  */

static void
btf_asm_enum_const (unsigned int size, ctf_dmdef_t * dmd, unsigned int idx)
{
  dw2_asm_output_data (4, dmd->dmd_name_offset, "ENUM_CONST '%s' idx=%u",
		       dmd->dmd_name, idx);
  if (size <= 4)
    dw2_asm_output_data (size < 4 ? 4 : size, dmd->dmd_value, "bte_value");
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
    dw2_asm_output_data (4, farg->farg_name_offset + stroffset,
			 "farg_name '%s'", farg->farg_name);
  else
    dw2_asm_output_data (4, 0, "farg_name ''");

  btf_asm_type_ref ("farg_type", farg->farg_type);
}

/* Asm'out a BTF_KIND_FUNC type.  */

static void
btf_asm_func_type (ctf_dtdef_ref dtd)
{
  dw2_asm_output_data (4, dtd->dtd_data.ctti_name,
		       "TYPE %" PRIu64 " BTF_KIND_FUNC '%s'",
		       dtd->dtd_type, get_btf_type_name (dtd));
  dw2_asm_output_data (4, BTF_TYPE_INFO (BTF_KIND_FUNC, 0, dtd->linkage),
		       "btt_info: kind=%u, kflag=%u, linkage=%u",
		       BTF_KIND_FUNC, 0, dtd->linkage);
  btf_asm_type_ref ("btt_type", dtd->ref_type);
}

/* Asm'out a variable entry following a BTF_KIND_DATASEC.  */

static void
btf_asm_datasec_entry (struct btf_datasec_entry entry)
{
  const char *symbol_name = NULL;
  if (entry.is_var)
    {
      symbol_name = entry.dvd->dvd_name;
      dw2_asm_output_data (4, entry.dvd->dvd_id,
			   "bts_type: (BTF_KIND_VAR '%s')", symbol_name);
    }
  else
    {
      symbol_name = entry.dtd->dtd_name;
      btf_asm_type_ref ("bts_type", entry.dtd);
    }

  if (!btf_with_core_debuginfo_p () || symbol_name == NULL)
    dw2_asm_output_data (4, 0, "bts_offset");
  else
    dw2_asm_output_offset (4, symbol_name, NULL, "bts_offset");

  dw2_asm_output_data (4, entry.size, "bts_size");
}

/* Asm'out a whole BTF_KIND_DATASEC, including its variable entries.  */

static void
btf_asm_datasec_type (btf_datasec_t ds)
{
  dw2_asm_output_data (4, ds.name_offset,
		       "TYPE %" PRIu64 " BTF_KIND_DATASEC '%s'",
		       ds.id, ds.name);
  dw2_asm_output_data (4, BTF_TYPE_INFO (BTF_KIND_DATASEC, 0,
					 ds.entries.length ()),
		       "btt_info: n_entries=%u", ds.entries.length ());
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

   if (!ctfc_is_empty_container (ctfc))
     {
       /* Total length (bytes) of the types section.  */
       type_len = ctfc->ctfc_num_types * sizeof (struct btf_type)
	 + ctfc->ctfc_num_vlen_bytes;

       str_off = type_off + type_len;

       str_len = ctfc->ctfc_strtable.ctstab_len
	 + ctfc->ctfc_aux_strtable.ctstab_len;
     }

   /* Offset of type section.  */
   dw2_asm_output_data (4, type_off, "type_off");
   /* Length of type section in bytes.  */
   dw2_asm_output_data (4, type_len, "type_len: ntypes=%u, vlen=%u",
			(uint32_t) ctfc->ctfc_num_types,
			(uint32_t) ctfc->ctfc_num_vlen_bytes);
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
  size_t num_ctf_vars = ctfc->ctfc_vars_list_count;
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
  static int str_pos = 0;

  while (ctf_string)
    {
      dw2_asm_output_nstring (ctf_string->cts_str, -1,
			      "btf_string, str_pos = 0x%x", str_pos);
      str_pos += strlen(ctf_string->cts_str) + 1;
      ctf_string = ctf_string->cts_next;
    }

  ctf_string = ctfc->ctfc_aux_strtable.ctstab_head;
  while (ctf_string)
    {
      dw2_asm_output_nstring (ctf_string->cts_str, -1,
			      "btf_aux_string, str_pos = 0x%x", str_pos);
      str_pos += strlen(ctf_string->cts_str) + 1;
      ctf_string = ctf_string->cts_next;
    }
}

/* Output all (representable) members of a BTF_KIND_STRUCT or
   BTF_KIND_UNION type.  */

static void
output_asm_btf_sou_fields (ctf_dtdef_ref dtd)
{
  ctf_dmdef_t * dmd;

  unsigned idx = 0;
  for (dmd = dtd->dtd_u.dtu_members;
       dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
    {
      btf_asm_sou_member (dmd, idx);
      idx++;
    }
}

/* Output all enumerator constants following a BTF_KIND_ENUM{,64}.  */

static void
output_asm_btf_enum_list (ctf_dtdef_ref dtd)
{
  ctf_dmdef_t * dmd;

  unsigned idx = 0;
  for (dmd = dtd->dtd_u.dtu_members;
       dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
    {
      btf_asm_enum_const (dtd->dtd_data.ctti_size, dmd, idx);
      idx++;
    }
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

  btf_kind = btf_dtd_kind (dtd);

  if (btf_kind == BTF_KIND_UNKN)
    return;

  switch (btf_kind)
    {
    case BTF_KIND_INT:
      /* Redundant definitions of void may still be hanging around in the type
	 list as size 0 integers. Skip emitting them.  */
      if (dtd->dtd_data.ctti_size < 1)
	break;

      /* In BTF the CHAR `encoding' seems to not be used, so clear it here.  */
      dtd->dtd_u.dtu_enc.cte_format &= ~BTF_INT_CHAR;

      encoding = BTF_INT_DATA (dtd->dtd_u.dtu_enc.cte_format,
			       dtd->dtd_u.dtu_enc.cte_offset,
			       dtd->dtd_u.dtu_enc.cte_bits);

      dw2_asm_output_data (4, encoding, "bti_encoding");
      break;

    case BTF_KIND_ARRAY:
      btf_asm_array (dtd->dtd_u.dtu_arr);
      break;

    case BTF_KIND_STRUCT:
    case BTF_KIND_UNION:
      output_asm_btf_sou_fields (dtd);
      break;

    case BTF_KIND_ENUM:
      output_asm_btf_enum_list (dtd);
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
  if (btf_emit_type_p (type))
    {
      btf_asm_type (type);
      output_asm_btf_vlen_bytes (ctfc, type);
    }
}

/* Output all BTF types in the container. This does not include synthesized
   types: BTF_KIND_VAR, BTF_KIND_FUNC, nor BTF_KIND_DATASEC.  */

static void
output_btf_types (ctf_container_ref ctfc)
{
  size_t i;
  size_t num_types;
  if (debug_prune_btf)
    num_types = max_translated_id;
  else
    num_types = ctfc->ctfc_types->elements ();

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
  ctf_dtdef_ref ref;
  unsigned i;
  FOR_EACH_VEC_ELT (*funcs, i, ref)
    btf_asm_func_type (ref);
}

/* Output all BTF_KIND_DATASEC records.  */

static void
output_btf_datasec_types (void)
{
  for (size_t i = 0; i < datasecs.length (); i++)
    btf_asm_datasec_type (datasecs[i]);
}

/* Write out all BTF debug info.  */

void
btf_output (ctf_container_ref ctfc)
{
  output_btf_header (ctfc);
  output_btf_types (ctfc);
  output_btf_vars (ctfc);
  output_btf_func_types ();
  output_btf_datasec_types ();
  output_btf_strs (ctfc);
}

/* Workaround for 'const void' variables.  These variables are sometimes used
   in eBPF programs to address kernel symbols.  DWARF does not generate const
   qualifier on void type, so we would incorrectly emit these variables
   without the const qualifier.  Find any such variables, and update them to
   refer to a new 'const' modifier type for void.  */

static void
btf_add_const_void (ctf_container_ref ctfc)
{
  ctf_dtdef_ref constvoid_dtd = NULL;
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

	  ctf_dvdef_ref dvd = ctf_dvd_lookup (ctfc, die);
	  if (dvd == NULL)
	    continue;

	  /* Create the 'const' modifier type for void.  */
	  if (constvoid_dtd == NULL)
	    constvoid_dtd = ctf_add_reftype (ctfc, CTF_ADD_ROOT,
					     dvd->dvd_type, CTF_K_CONST, NULL);
	  dvd->dvd_type = constvoid_dtd;
	}
    }
}

/* Functions actually get two type records: a BTF_KIND_FUNC_PROTO, and also a
   BTF_KIND_FUNC.  But the CTF container only allocates one type per function,
   which matches closely with BTF_KIND_FUNC_PROTO.  For each such function,
   construct a BTF_KIND_FUNC entry.  This is done early, because we want FUNC
   records even for functions which are later inlined by optimizations.  */

static void
btf_add_func_records (ctf_container_ref ctfc)
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

      /* Do not add FUNC records for kernel helpers.  */
      if (DECL_EXTERNAL (func->decl)
	  && (lookup_attribute ("kernel_helper",
				DECL_ATTRIBUTES (func->decl))) != NULL_TREE)
	continue;

      ctf_dtdef_ref func_dtd = ggc_cleared_alloc<ctf_dtdef_t> ();
      func_dtd->dtd_data = dtd->dtd_data;
      func_dtd->dtd_data.ctti_type = dtd->dtd_type;
      func_dtd->ref_type = dtd;
      func_dtd->linkage = dtd->linkage;
      func_dtd->dtd_name = dtd->dtd_name;
      /* Type ID will be assigned just before output.  */

      /* Only the BTF_KIND_FUNC type actually references the name.
	 The BTF_KIND_FUNC_PROTO is always anonymous.  */
      dtd->dtd_data.ctti_name = 0;

      /* Mark 'extern' funcs.  */
      if (DECL_EXTERNAL (func->decl))
	func_dtd->linkage = BTF_FUNC_EXTERN;

      /* Buffer newly created FUNC records.  We cannot simply insert them
	 into the types map, because types are keyed by their DWARF DIE,
	 and we have no unique DIE to use as a key since the FUNC_PROTOs
	 are already present in the map.  */
      vec_safe_push (funcs, func_dtd);
      func_map->put (dtd, func_dtd);
    }
}

/* The set of types used directly in the source program, and any types manually
   marked as used.  This is the set of types which will be emitted when
   flag_prune_btf is set.  */
static GTY (()) hash_set<ctf_dtdef_ref> *btf_used_types;

/* Fixup used to avoid unnecessary pointer chasing for types.  A fixup is
   created when a structure or union member is a pointer to another struct
   or union type.  In such cases, avoid emitting full type information for
   the pointee struct or union type (which may be quite large), unless that
   type is used directly elsewhere.  */
struct btf_fixup
{
  ctf_dtdef_ref pointer_dtd; /* Type node to which the fixup is applied.  */
  ctf_dtdef_ref pointee_dtd; /* Original type node referred to by pointer_dtd.
				If this concrete type is not otherwise used,
				then a forward is created.  */
};

/* Stores fixups while processing types.  */
static vec<struct btf_fixup> fixups;

/* For fixups where the underlying type is not used in the end, a BTF_KIND_FWD
   is created and emitted.  This vector stores them.  */
static GTY (()) vec<ctf_dtdef_ref, va_gc> *forwards;

/* Recursively add type DTD and any types it references to the used set.
   Return a type that should be used for references to DTD - usually DTD itself,
   but may be NULL if DTD corresponds to a type which will not be emitted.
   CHECK_PTR is true if one of the predecessors in recursive calls is a struct
   or union member.  SEEN_PTR is true if CHECK_PTR is true AND one of the
   predecessors was a pointer type.  These two flags are used to avoid chasing
   pointers to struct/union only used from pointer members.  For such types, we
   will emit a forward instead of the full type information, unless
   CREATE_FIXUPS is false.  */

static ctf_dtdef_ref
btf_add_used_type (ctf_container_ref ctfc, ctf_dtdef_ref dtd,
		   bool check_ptr, bool seen_ptr, bool create_fixups)
{
  if (dtd == NULL)
    return NULL;

  uint32_t ctf_kind = CTF_V2_INFO_KIND (dtd->dtd_data.ctti_info);
  uint32_t kind = get_btf_kind (ctf_kind);

  /* Check whether the type has already been added.  */
  if (btf_used_types->contains (dtd))
    {
      /* It's possible the type was already added as a fixup, but that we now
	 have a concrete use of it.  */
      switch (kind)
	{
	case BTF_KIND_PTR:
	case BTF_KIND_TYPEDEF:
	case BTF_KIND_CONST:
	case BTF_KIND_VOLATILE:
	case BTF_KIND_RESTRICT:
	  if (check_ptr)
	    /* Type was previously added as a fixup, and that's OK.  */
	    return dtd;
	  else
	    {
	      /* The type was previously added as a fixup, but now we have
		 a concrete use of it.  Remove the fixup.  */
	      for (size_t i = 0; i < fixups.length (); i++)
		if (fixups[i].pointer_dtd == dtd)
		  fixups.unordered_remove (i);

	      /* Add the concrete base type.  */
	      dtd->ref_type = btf_add_used_type (ctfc, dtd->ref_type, check_ptr,
						 seen_ptr, create_fixups);
	      return dtd;
	    }
	default:
	  return dtd;
	}
    }

  if (ctf_kind == CTF_K_SLICE)
    {
      /* Bitfield.  Add the underlying type to the used set, but leave
	 the reference to the bitfield.  The slice type won't be emitted,
	 but we need the information in it when writing out the bitfield
	 encoding.  */
      btf_add_used_type (ctfc, dtd->dtd_u.dtu_slice.cts_type,
			 check_ptr, seen_ptr, create_fixups);
      return dtd;
    }

  /* Skip redundant definitions of void and types with no BTF encoding.  */
  if ((kind == BTF_KIND_INT && dtd->dtd_data.ctti_size == 0)
      || (kind == BTF_KIND_UNKN))
    return NULL;

  /* Add the type itself, and assign its id.
     Do this before recursing to handle things like linked list structures.  */
  gcc_assert (ctfc->ctfc_nextid <= BTF_MAX_TYPE);
  dtd->dtd_type = ctfc->ctfc_nextid++;
  btf_used_types->add (dtd);
  ctf_add_string (ctfc, dtd->dtd_name, &(dtd->dtd_data.ctti_name), CTF_STRTAB);
  ctfc->ctfc_num_types++;
  ctfc->ctfc_num_vlen_bytes += btf_calc_num_vbytes (dtd);

  /* Recursively add types referenced by this type.  */
  switch (kind)
    {
    case BTF_KIND_INT:
    case BTF_KIND_FLOAT:
    case BTF_KIND_FWD:
      /* Leaf kinds which do not refer to any other types.  */
      break;

    case BTF_KIND_FUNC:
    case BTF_KIND_VAR:
      /* Root kinds; no type we are visiting may refer to these.  */
      gcc_unreachable ();

    case BTF_KIND_PTR:
    case BTF_KIND_TYPEDEF:
    case BTF_KIND_CONST:
    case BTF_KIND_VOLATILE:
    case BTF_KIND_RESTRICT:
      {
	/* These type kinds refer to exactly one other type.  */
	if (check_ptr && !seen_ptr)
	  seen_ptr = (kind == BTF_KIND_PTR);

	/* Try to avoid chasing pointers to struct/union types if the
	   underlying type isn't used.  */
	if (check_ptr && seen_ptr && create_fixups)
	  {
	    ctf_dtdef_ref ref = dtd->ref_type;
	    uint32_t ref_kind = btf_dtd_kind (ref);

	    if ((ref_kind == BTF_KIND_STRUCT || ref_kind == BTF_KIND_UNION)
		&& !btf_used_types->contains (ref))
	      {
		struct btf_fixup fixup;
		fixup.pointer_dtd = dtd;
		fixup.pointee_dtd = ref;
		fixups.safe_push (fixup);
		break;
	      }
	  }

	/* Add the type to which this type refers.  */
	dtd->ref_type = btf_add_used_type (ctfc, dtd->ref_type, check_ptr,
					   seen_ptr, create_fixups);
	break;
      }
    case BTF_KIND_ARRAY:
      {
	/* Add element and index types.  */
	ctf_arinfo_t *arr = &(dtd->dtd_u.dtu_arr);
	arr->ctr_contents = btf_add_used_type (ctfc, arr->ctr_contents, false,
					       false, create_fixups);
	arr->ctr_index = btf_add_used_type (ctfc, arr->ctr_index, false, false,
					    create_fixups);
	break;
      }
    case BTF_KIND_STRUCT:
    case BTF_KIND_UNION:
    case BTF_KIND_ENUM:
    case BTF_KIND_ENUM64:
      {
	/* Add members.  */
	ctf_dmdef_t *dmd;
	for (dmd = dtd->dtd_u.dtu_members;
	     dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
	  {
	    /* Add member type for struct/union members.  For enums, only the
	       enumerator names are needed.  */
	    if (kind == BTF_KIND_STRUCT || kind == BTF_KIND_UNION)
	      dmd->dmd_type = btf_add_used_type (ctfc, dmd->dmd_type, true,
						 false, create_fixups);
	    ctf_add_string (ctfc, dmd->dmd_name, &(dmd->dmd_name_offset),
			    CTF_STRTAB);
	  }
	break;
      }
    case BTF_KIND_FUNC_PROTO:
      {
	/* Add return type.  */
	dtd->ref_type = btf_add_used_type (ctfc, dtd->ref_type, false, false,
					   create_fixups);

	/* Add arg types.  */
	ctf_func_arg_t * farg;
	for (farg = dtd->dtd_u.dtu_argv;
	     farg != NULL; farg = (ctf_func_arg_t *) ctf_farg_list_next (farg))
	  {
	    farg->farg_type = btf_add_used_type (ctfc, farg->farg_type, false,
						 false, create_fixups);
	    /* Note: argument names are stored in the auxilliary string table,
	       since CTF does not include arg names.  That table has not been
	       cleared, so no need to re-add argument names here.  */
	  }
	break;
      }
    default:
      return NULL;
    }

  return dtd;
}

/* Initial entry point of BTF generation, called at early_finish () after
   CTF information has possibly been output.  Translate all CTF information
   to BTF, and do any processing that must be done early, such as creating
   BTF_KIND_FUNC records.  */

void
btf_early_finish (void)
{
  ctf_container_ref tu_ctfc = ctf_get_tu_ctfc ();

  vec_alloc (funcs, 16);
  func_map = hash_map<ctf_dtdef_ref, ctf_dtdef_ref>::create_ggc (16);

  /* Note: from this point on, destructive changes are made to the TU CTFC to
     translate CTF to BTF.  If CTF debug info has also been requested, it must
     be emitted before starting the translation to BTF.  */
  btf_add_const_void (tu_ctfc);
  btf_add_func_records (tu_ctfc);

  /* These fields are reset to count BTF types etc.  */
  tu_ctfc->ctfc_num_types = 0;
  tu_ctfc->ctfc_num_vlen_bytes = 0;
  tu_ctfc->ctfc_vars_list_count = 0;

  if (debug_prune_btf)
    {
      btf_used_types
	= hash_set<ctf_dtdef_ref>::create_ggc (tu_ctfc->ctfc_types->elements ());
      tu_ctfc->ctfc_nextid = 1;
      fixups.create (1);

      /* Empty the string table, which was already populated with strings for
	 all types translated from DWARF.  We may only need a very small subset
	 of these strings; those will be re-added below.  */
      ctfc_delete_strtab (&tu_ctfc->ctfc_strtable);
      init_ctf_strtable (&tu_ctfc->ctfc_strtable);
      tu_ctfc->ctfc_strlen++;
    }
}

/* Push a BTF datasec entry ENTRY into the datasec named SECNAME,
   creating the datasec record if it does not already exist.  */

static void
btf_datasec_push_entry (ctf_container_ref ctfc, const char *secname,
			struct btf_datasec_entry entry)
{
  if (secname == NULL)
    return;

  /* If we already have a datasec record for the appropriate section,
     append the new entry to it.  */
  for (size_t i = 0; i < datasecs.length (); i++)
    if (strcmp (datasecs[i].name, secname) == 0)
      {
	datasecs[i].entries.safe_push (entry);
	return;
      }

  /* If we don't already have a datasec record for secname, make one.  */
  uint32_t str_off;
  ctf_add_string (ctfc, secname, &str_off, CTF_AUX_STRTAB);
  if (strcmp (secname, ""))
    ctfc->ctfc_aux_strlen += strlen (secname) + 1;

  /* Note: ID will be assigned just before output.  */
  btf_datasec_t ds;
  ds.name = secname;
  ds.name_offset = str_off;

  /* Insert the entry into the new datasec record.  */
  ds.entries.create (1);
  ds.entries.quick_push (entry);

  /* Insert the datasec record itself.  */
  datasecs.safe_push (ds);
}

/* Create a datasec entry for a function, and insert it into the datasec
   record for the appropriate section.  Create the record if it does not
   yet exist.  */

static void
btf_datasec_add_func (ctf_container_ref ctfc, cgraph_node *func,
		      ctf_dtdef_ref func_dtd)
{
  const char *section_name = get_section_name (func);

  /* Note: get_section_name () returns NULL for functions in text
     section.  This is intentional, since we do not want to generate
     DATASEC entries for them.  */
  if (section_name == NULL)
    return;

  struct btf_datasec_entry entry;
  gcc_assert (func_dtd);
  entry.dtd = func_dtd;
  entry.is_var = false;

  /* Size is left as zero at compile time, to be filled in by loaders
     such as libbpf.  */
  entry.size = 0;

  btf_datasec_push_entry (ctfc, section_name, entry);
}

/* Create a datasec entry for a variable, and insert it into the datasec
   record for the appropriate section.  Create the record if it does not
   yet exist.  */

static void
btf_datasec_add_var (ctf_container_ref ctfc, varpool_node *var,
		     ctf_dvdef_ref dvd)
{
  /* PR112849: avoid assuming a section for extern decls without
     an explicit section, which would result in incorrectly
     emitting a BTF_KIND_DATASEC entry for them.  */
  if (DECL_EXTERNAL (var->decl) && var->get_section () == NULL)
    return;

  const char *section_name = get_section_name (var);
  if (section_name == NULL)
    return;

  gcc_assert (dvd);
  struct btf_datasec_entry entry;
  entry.dvd = dvd;
  entry.is_var = true;
  entry.size = 0;

  tree size = DECL_SIZE_UNIT (var->decl);
  if (tree_fits_uhwi_p (size))
    entry.size = tree_to_uhwi (size);
  else if (VOID_TYPE_P (TREE_TYPE (var->decl)))
    entry.size = 1;

  btf_datasec_push_entry (ctfc, section_name, entry);
}

/* Add datasec entries for functions to CTFC.  */

static void
btf_add_func_datasec_entries (ctf_container_ref ctfc)
{
  /* We need to create FUNC records at early_finish, so that we have them
     even for functions which are later inlined by optimization passes.
     But on the other hand, we do not want datasec entries for such functions,
     so only create the datasec entries for them late.  This loop will not
     hit functions which have already been inlined.  */
  cgraph_node *func;
  FOR_EACH_FUNCTION (func)
    {
      dw_die_ref die = lookup_decl_die (func->decl);
      if (die == NULL)
	continue;

      ctf_dtdef_ref dtd = ctf_dtd_lookup (ctfc, die);
      if (dtd == NULL)
	continue;

      ctf_dtdef_ref *pdtd = func_map->get (dtd);
      if (pdtd && DECL_EXTERNAL (func->decl))
	btf_datasec_add_func (ctfc, func, *pdtd);
    }
}

/* Helper function used to determine whether or not a BTF_KIND_VAR record
   for the variable VAR shall be emitted.  */

static bool
btf_emit_variable_p (ctf_container_ref ctfc, varpool_node *var,
		     ctf_dvdef_ref *pdvd)
{
  dw_die_ref die = lookup_decl_die (var->decl);
  if (die == NULL)
    return false;

  ctf_dvdef_ref dvd = ctf_dvd_lookup (ctfc, die);
  if (dvd == NULL)
    return false;

  /* If this is an extern variable declaration with a defining declaration
     later, skip it so that only the defining declaration is emitted.
     This is the same case, fix and reasoning as in CTF; see PR105089.  */
  if (ctf_dvd_ignore_lookup (ctfc, dvd->dvd_key))
    return false;

  /* Skip variables with unrepresentable types.  */
  if (!btf_emit_type_p (dvd->dvd_type))
    return false;

  *pdvd = dvd;
  return true;
}

/* Add BTF_KIND_VAR records for variables.  */

static void
btf_add_vars (ctf_container_ref ctfc)
{
  size_t num_ctf_vars = ctfc->ctfc_vars->elements ();

  ctfc->ctfc_vars_list = ggc_vec_alloc<ctf_dvdef_ref>(num_ctf_vars);

  varpool_node *var;
  ctf_dvdef_ref dvd;
  FOR_EACH_VARIABLE (var)
    {
      if (!btf_emit_variable_p (ctfc, var, &dvd))
	continue;

      /* Mark 'extern' variables.  */
      if (DECL_EXTERNAL (var->decl))
	dvd->dvd_visibility = BTF_VAR_GLOBAL_EXTERN;

      /* Add the variable to the vars list.  */
      ctfc->ctfc_vars_list[ctfc->ctfc_vars_list_count++] = dvd;

      /* Add a BTF_KIND_DATASEC entry for the variable.  */
      btf_datasec_add_var (ctfc, var, dvd);

      const char *section = var->get_section ();
      if (section && (strcmp (section, ".maps") == 0) && debug_prune_btf)
	{
	  /* The .maps section has special meaning in BTF: it is used for BPF
	     map definitions.  These definitions should be structs.  We must
	     collect pointee types used in map members as though they are used
	     directly, effectively ignoring (from the pruning perspective) that
	     they are struct members.  */
	  ctf_dtdef_ref dtd = dvd->dvd_type;
	  uint32_t kind = btf_dtd_kind (dvd->dvd_type);
	  if (kind == BTF_KIND_STRUCT)
	    {
	      ctf_dmdef_t *dmd;
	      for (dmd = dtd->dtd_u.dtu_members;
		   dmd != NULL; dmd = (ctf_dmdef_t *) ctf_dmd_list_next (dmd))
		btf_add_used_type (ctfc, dmd->dmd_type, false, false, true);
	    }
	}
    }
}

/* Callback used by btf_assign_type_ids to insert types into their initial
   positions in the type list.  */

static int
btf_type_list_cb (ctf_dtdef_ref *slot, ctf_container_ref ctfc)
{
  ctf_dtdef_ref dtd = *slot;
  ctfc->ctfc_types_list[dtd->dtd_type] = dtd;
  return 1;
}

/* Construct the initial type list and assign BTF IDs for all types translated
   from CTF.  */

static void
btf_collect_translated_types (ctf_container_ref ctfc)
{
  size_t num_ctf_types = ctfc->ctfc_types->elements ();

  /* First, place each type at its CTF-assigned index in the list.
     The '+1' here and below is to account for the implicit void type with
     ID 0.  There is no real type at index 0 in the list.  */
  ctfc->ctfc_types_list = ggc_vec_alloc<ctf_dtdef_ref>(num_ctf_types + 1);
  ctfc->ctfc_types->traverse<ctf_container_ref, btf_type_list_cb> (ctfc);

  /* Now, pass through the list and adjust IDs to account for types which will
     not be emitted.  This results in each type that will be emitted in BTF
     being assigned an appropriate ID.  Note that types which will not be
     emitted remain in the list; they are skipped at output time.  */
  unsigned int skip = 0;
  for (size_t i = 1; i <= num_ctf_types; i++)
    {
      ctf_dtdef_ref dtd = ctfc->ctfc_types_list[i];
      if (!btf_emit_type_p (dtd))
	{
	  dtd->dtd_type = BTF_INVALID_TYPEID;
	  skip += 1;
	  continue;
	}

      dtd->dtd_type -= skip;
      ctfc->ctfc_num_types++;
      ctfc->ctfc_num_vlen_bytes += btf_calc_num_vbytes (dtd);
    }

  max_translated_id = ctfc->ctfc_num_types;
  ctfc->ctfc_nextid = ctfc->ctfc_num_types + 1;
}

/* Assign BTF IDs for FUNC records and account for their size.  */

static void
btf_assign_func_ids (ctf_container_ref ctfc)
{
  ctf_dtdef_ref dtd;
  unsigned int i;
  FOR_EACH_VEC_ELT (*funcs, i, dtd)
    {
      dtd->dtd_type = ctfc->ctfc_nextid++;
      ctfc->ctfc_num_types++;
    }
}

/* Assign BTF IDs for variables and account for their size.  */

static void
btf_assign_var_ids (ctf_container_ref ctfc)
{
  for (size_t i = 0; i < ctfc->ctfc_vars_list_count; i++)
    {
      ctf_dvdef_ref dvd = ctfc->ctfc_vars_list[i];
      ctf_id_t id = ctfc->ctfc_nextid++;
      gcc_assert (id <= BTF_MAX_TYPE);
      dvd->dvd_id = id;

      ctfc->ctfc_num_types++;
      ctfc->ctfc_num_vlen_bytes += sizeof (struct btf_var);
    }
}

/* Assign BTF IDs for datasec records and account for their size.  */

static void
btf_assign_datasec_ids (ctf_container_ref ctfc)
{
  for (size_t i = 0; i < datasecs.length (); i++)
    {
      datasecs[i].id = ctfc->ctfc_nextid++;
      datasecs[i].name_offset += ctfc_get_strtab_len (ctfc, CTF_STRTAB);
      ctfc->ctfc_num_types++;
      ctfc->ctfc_num_vlen_bytes += (datasecs[i].entries.length ()
				    * sizeof (struct btf_var_secinfo));
    }
}


/* Manually mark that type T is used to ensure it will not be pruned.
   Used by the BPF backend when generating BPF CO-RE to mark types used
   in CO-RE relocations.  */

void
btf_mark_type_used (tree t)
{
  /* If we are not going to prune anyway, this is a no-op.  */
  if (!debug_prune_btf)
    return;

  gcc_assert (TYPE_P (t));
  ctf_container_ref ctfc = ctf_get_tu_ctfc ();
  ctf_dtdef_ref dtd = ctf_lookup_tree_type (ctfc, t);

  if (!dtd)
    return;

  btf_add_used_type (ctfc, dtd, false, false, true);
}

/* Callback used for assembling the only-used-types list.  Note that this is
   the same as btf_type_list_cb above, but the hash_set traverse requires a
   different function signature.  */

static bool
btf_used_type_list_cb (const ctf_dtdef_ref& dtd, ctf_container_ref ctfc)
{
  ctfc->ctfc_types_list[dtd->dtd_type] = dtd;
  return true;
}

/* Collect the set of types reachable from global variables and functions.
   This is the minimal set of types, used when generating pruned BTF.  */

static void
btf_collect_pruned_types (ctf_container_ref ctfc)
{
  vec_alloc (forwards, 1);

  /* Add types used from functions.  */
  ctf_dtdef_ref dtd;
  size_t i;
  FOR_EACH_VEC_ELT (*funcs, i, dtd)
    {
      btf_add_used_type (ctfc, dtd->ref_type, false, false, true);
      ctf_add_string (ctfc, dtd->dtd_name, &(dtd->dtd_data.ctti_name),
		      CTF_STRTAB);
    }

  /* Add types used from global variables.  */
  for (i = 0; i < ctfc->ctfc_vars_list_count; i++)
    {
      ctf_dvdef_ref dvd = ctfc->ctfc_vars_list[i];
      btf_add_used_type (ctfc, dvd->dvd_type, false, false, true);
      ctf_add_string (ctfc, dvd->dvd_name, &(dvd->dvd_name_offset), CTF_STRTAB);
    }

  /* Process fixups.  If the base type was never added, create a forward for it
     and adjust the reference to point to that.  If it was added, then nothing
     needs to change.  */
  for (i = 0; i < fixups.length (); i++)
    {
      struct btf_fixup *fx = &fixups[i];
      if (!btf_used_types->contains (fx->pointee_dtd))
	{
	  /* The underlying type is not used.  Create a forward.  */
	  ctf_dtdef_ref fwd = ggc_cleared_alloc<ctf_dtdef_t> ();
	  ctf_id_t id = ctfc->ctfc_nextid++;
	  gcc_assert (id <= BTF_MAX_TYPE);

	  bool union_p = (btf_dtd_kind (fx->pointee_dtd) == BTF_KIND_UNION);

	  fwd->dtd_name = fx->pointee_dtd->dtd_name;
	  fwd->dtd_data.ctti_info = CTF_TYPE_INFO (CTF_K_FORWARD, union_p, 0);
	  fwd->dtd_type = id;
	  ctfc->ctfc_num_types++;
	  ctfc->ctfc_num_vlen_bytes += btf_calc_num_vbytes (fwd);
	  ctf_add_string (ctfc, fwd->dtd_name, &(fwd->dtd_data.ctti_name),
			  CTF_STRTAB);

	  /* Update the pointer to point to the forward.  */
	  fx->pointer_dtd->ref_type = fwd;
	  vec_safe_push (forwards, fwd);
	}
    }

  /* Construct the resulting pruned type list.  */
  ctfc->ctfc_types_list
    = ggc_vec_alloc<ctf_dtdef_ref> (btf_used_types->elements () + 1
				    + vec_safe_length (forwards));

  btf_used_types->traverse<ctf_container_ref, btf_used_type_list_cb> (ctfc);

  /* Insert the newly created forwards into the regular types list too.  */
  FOR_EACH_VEC_ELT (*forwards, i, dtd)
    ctfc->ctfc_types_list[dtd->dtd_type] = dtd;

  max_translated_id = btf_used_types->elements () + vec_safe_length (forwards);
}

/* Late entry point for BTF generation, called from dwarf2out_finish ().
   Complete and emit BTF information.  */

void
btf_finish (void)
{
  ctf_container_ref tu_ctfc = ctf_get_tu_ctfc ();
  init_btf_sections ();

  datasecs.create (0);

  btf_add_vars (tu_ctfc);
  if (debug_prune_btf)
    {
      /* Collect pruned set of BTF types and prepare for emission.
	 This includes only types directly used in file-scope variables and
	 function return/argument types.  */
      btf_collect_pruned_types (tu_ctfc);
    }
  else
    {
      /* Collect all BTF types and prepare for emission.
	 This includes all types translated from DWARF.  */
      btf_collect_translated_types (tu_ctfc);
    }
  btf_add_func_datasec_entries (tu_ctfc);

  btf_assign_var_ids (tu_ctfc);
  btf_assign_func_ids (tu_ctfc);
  btf_assign_datasec_ids (tu_ctfc);

  /* Finally, write out the complete .BTF section.  */
  btf_output (tu_ctfc);

  /* If compiling for BPF with CO-RE info, we cannot deallocate until after the
     contents of the .BTF.ext section are finalized, which happens very late in
     BPF backend.  Therefore, the deallocation (i.e. btf_finalize ()) is delayed
     until TARGET_ASM_FILE_END for BPF CO-RE.  */
  if (!btf_with_core_debuginfo_p ())
    btf_finalize ();
}

/* Reset all state for BTF generation so that we can rerun the compiler within
   the same process.  */

void
btf_finalize (void)
{
  btf_info_section = NULL;
  max_translated_id = 0;

  for (size_t i = 0; i < datasecs.length (); i++)
    datasecs[i].entries.release ();
  datasecs.release ();

  funcs = NULL;
  if (func_map)
    {
      func_map->empty ();
      func_map = NULL;
    }

  if (debug_prune_btf)
    {
      if (btf_used_types)
	{
	  btf_used_types->empty ();
	  btf_used_types = NULL;
	}

      fixups.release ();
      forwards = NULL;
    }

  ctf_container_ref tu_ctfc = ctf_get_tu_ctfc ();
  ctfc_delete_container (tu_ctfc);
  tu_ctfc = NULL;
}

/* Traversal function for all BTF_KIND_FUNC type records.  */

bool
traverse_btf_func_types (funcs_traverse_callback callback, void *data)
{
  ctf_dtdef_ref ref;
  unsigned i;
  FOR_EACH_VEC_ELT (*funcs, i, ref)
    {
      bool stop = callback (ref, data);
      if (stop == true)
	return true;
    }
  return false;
}

#include "gt-btfout.h"
