/* Generate CTF.
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
#include "toplev.h"
#include "ctfc.h"
#include "diagnostic-core.h"

/* A CTF container object - one per translation unit.  */

ctf_container_ref tu_ctfc;

ctf_container_ref
ctf_get_tu_ctfc (void)
{
  return tu_ctfc;
}

/* If the next ctf type id is still set to the init value, no ctf records to
   report.  */
bool
ctfc_is_empty_container (ctf_container_ref ctfc)
{
  return ((ctfc)->ctfc_nextid == CTF_INIT_TYPEID);
}

/* Get the total number of CTF types in the container.  */

unsigned int
ctfc_get_num_ctf_types (ctf_container_ref ctfc)
{
  return ctfc->ctfc_types->elements ();
}

/* Get the total number of CTF variables in the container.  */

unsigned int ctfc_get_num_ctf_vars (ctf_container_ref ctfc)
{
  return ctfc->ctfc_vars->elements ();
}

/* Get reference to the CTF string table or the CTF auxilliary
   string table.  */

ctf_strtable_t *
ctfc_get_strtab (ctf_container_ref ctfc, int aux)
{
  return aux ? &(ctfc)->ctfc_aux_strtable : &(ctfc->ctfc_strtable);
}

/* Get the length of the specified string table of the CTF container.  */

size_t
ctfc_get_strtab_len (ctf_container_ref ctfc, int aux)
{
  ctf_strtable_t * strtab = ctfc_get_strtab (ctfc, aux);
  return strtab->ctstab_len;
}

/* Get the number of bytes to represent the variable length portion of all CTF
   types in the CTF container.  */

size_t ctfc_get_num_vlen_bytes (ctf_container_ref ctfc)
{
  return ctfc->ctfc_num_vlen_bytes;
}

/* Return which member of the union is used in CTFTYPE.  Used for garbage
   collection.  */

enum ctf_dtu_d_union_enum
ctf_dtu_d_union_selector (ctf_dtdef_ref ctftype)
{
  uint32_t kind = CTF_V2_INFO_KIND (ctftype->dtd_data.ctti_info);
  switch (kind)
    {
    case CTF_K_UNKNOWN:
    case CTF_K_INTEGER:
    case CTF_K_FLOAT:
      return CTF_DTU_D_ENCODING;
    case CTF_K_STRUCT:
    case CTF_K_UNION:
    case CTF_K_ENUM:
      return CTF_DTU_D_MEMBERS;
    case CTF_K_ARRAY:
      return CTF_DTU_D_ARRAY;
    case CTF_K_FUNCTION:
      return CTF_DTU_D_ARGUMENTS;
    case CTF_K_SLICE:
      return CTF_DTU_D_SLICE;
    default:
      /* The largest member as default.  */
      return CTF_DTU_D_ARRAY;
    }
}

/* Insert CTF type into the CTF container.  */

static void
ctf_dtd_insert (ctf_container_ref ctfc, ctf_dtdef_ref dtd)
{
  bool existed = false;
  ctf_dtdef_ref entry = dtd;

  ctf_dtdef_ref * item = ctfc->ctfc_types->find_slot (entry, INSERT);
  if (*item == NULL)
     *item = dtd;
  else
    existed = true;
  /* Duplicate CTF type records not expected to be inserted.  */
  gcc_assert (!existed);
}

/* Lookup CTF type given a DWARF die for the type.  */

ctf_dtdef_ref
ctf_dtd_lookup (const ctf_container_ref ctfc, const dw_die_ref type)
{
  ctf_dtdef_t entry;
  entry.dtd_key = type;

  ctf_dtdef_ref * slot = ctfc->ctfc_types->find_slot (&entry, NO_INSERT);

  if (slot)
    return (ctf_dtdef_ref)*slot;

  return NULL;
}

/* Insert CTF variable into the CTF container.  */

static void
ctf_dvd_insert (ctf_container_ref ctfc, ctf_dvdef_ref dvd)
{
  bool existed = false;
  ctf_dvdef_ref entry = dvd;

  ctf_dvdef_ref * item = ctfc->ctfc_vars->find_slot (entry, INSERT);
  if (*item == NULL)
     *item = dvd;
  else
    existed = true;
  /* Duplicate variable records not expected to be inserted.  */
  gcc_assert (!existed);
}

/* Lookup CTF variable given a DWARF die for the decl.  */

ctf_dvdef_ref
ctf_dvd_lookup (const ctf_container_ref ctfc, dw_die_ref die)
{
  ctf_dvdef_t entry;
  entry.dvd_key = die;

  ctf_dvdef_ref * slot = ctfc->ctfc_vars->find_slot (&entry, NO_INSERT);

  if (slot)
    return (ctf_dvdef_ref)*slot;

  return NULL;
}

/* Insert a dummy CTF variable into the list of variables to be ignored.  */

static void
ctf_dvd_ignore_insert (ctf_container_ref ctfc, ctf_dvdef_ref dvd)
{
  bool existed = false;
  ctf_dvdef_ref entry = dvd;

  ctf_dvdef_ref * item = ctfc->ctfc_ignore_vars->find_slot (entry, INSERT);
  if (*item == NULL)
     *item = dvd;
  else
    existed = true;
  /* Duplicate variable records not expected to be inserted.  */
  gcc_assert (!existed);
}

/* Lookup the dummy CTF variable given the DWARF die for the non-defining
   decl to be ignored.  */

bool
ctf_dvd_ignore_lookup (const ctf_container_ref ctfc, dw_die_ref die)
{
  ctf_dvdef_t entry;
  entry.dvd_key = die;

  ctf_dvdef_ref * slot = ctfc->ctfc_ignore_vars->find_slot (&entry, NO_INSERT);

  if (slot)
    return true;

  return false;
}

/* Append member definition to the list.  Member list is a singly-linked list
   with list start pointing to the head.  */

static void
ctf_dmd_list_append (ctf_dmdef_t ** dmd, ctf_dmdef_t * elem)
{
  ctf_dmdef_t * tail = (dmd && *dmd) ? *dmd : NULL;
  if (tail)
    {
      while (tail->dmd_next)
	tail = tail->dmd_next;

      tail->dmd_next = elem;
    }
  else
    *dmd = elem;

  elem->dmd_next = NULL;
}

/* Append function argument to the list.  Member list is a singly-linked list
   with list start pointing to the head.  */

static void
ctf_farg_list_append (ctf_func_arg_t ** farg, ctf_func_arg_t * elem)
{
  ctf_func_arg_t * tail = (farg && *farg) ? *farg : NULL;
  if (tail)
    {
      while (tail->farg_next)
	tail = tail->farg_next;

      tail->farg_next = elem;
    }
  else
    *farg = elem;

  elem->farg_next = NULL;
}

/* Append str to the CTF string table.  */

static void
ctfc_strtable_append_str (ctf_strtable_t * str_table, const char * str)
{
  ctf_string_t * ctf_string = ggc_cleared_alloc<ctf_string_t> ();
  /* Keep a reference to the input STR.  */
  ctf_string->cts_str = str;
  ctf_string->cts_next = NULL;

  if (!str_table->ctstab_head)
    str_table->ctstab_head = ctf_string;

  /* Append to the end of the list.  */
  if (str_table->ctstab_tail)
    str_table->ctstab_tail->cts_next = ctf_string;

  str_table->ctstab_tail = ctf_string;
}

/* Wrapper function to add str to the CTF string table.  No de-duplication of
   CTF strings is done by the compiler.  */

static const char *
ctfc_strtable_add_str (ctf_strtable_t * str_table, const char * name,
		      uint32_t * name_offset)
{
  size_t len;
  char * ctf_string;
  /* Return value is the offset to the string in the string table.  */
  uint32_t str_offset = str_table->ctstab_len;

  /* Add empty string only once at the beginning of the string table.  Also, do
     not add null strings, return the offset to the empty string for them.  */
  if ((!name || (name != NULL && !strcmp (name, ""))) && str_offset)
    {
      ctf_string = CONST_CAST (char *, str_table->ctstab_estr);
      str_offset = 0;
    }
  else
    {
      gcc_assert (name);
      /* Add null-terminated strings to the string table.  */
      len = strlen (name) + 1;
      ctf_string = CONST_CAST (char *, ggc_strdup (name));

      ctfc_strtable_append_str (str_table, ctf_string);
      /* Add string to the string table.  Keep number of strings updated.  */
      str_table->ctstab_num++;
      /* Keep the number of bytes contained in the string table updated.  */
      str_table->ctstab_len += len;
    }

  *name_offset = str_offset;

  return (const char *) ctf_string;

}

/* Add string to the appropriate string table in the CTF container.  */

const char *
ctf_add_string (ctf_container_ref ctfc, const char * name,
		uint32_t * name_offset, int aux_str = CTF_STRTAB)
{
  /* Get the CTF string table or the CTF auxilliary string table,
     as applicable.  */
  ctf_strtable_t *str_table = ctfc_get_strtab (ctfc, aux_str);
  return ctfc_strtable_add_str (str_table, name, name_offset);
}

/* Add the compilation unit (CU) name string to the CTF string table.  The
   CU name has a prepended pwd string if it is a relative path.  Also set the
   CU name offset in the CTF container.  */

void
ctf_add_cuname (ctf_container_ref ctfc, const char * filename)
{
  char * cuname = NULL;

  /* (filename at this point of compilation cannot be null).  */

  if (!IS_DIR_SEPARATOR (filename[0]))
    {
      /* Filename is a relative path.  */
      const char * cu_pwd = get_src_pwd ();
      const int cu_pwd_len = strlen (cu_pwd);

      /* Add a DIR_SEPARATOR char before the filename.  */
      const int len = cu_pwd_len + 2 + strlen (filename);

      cuname = (char *) ggc_alloc_atomic (len);
      memset (cuname, 0, len);

      strcpy (cuname, cu_pwd);
      cuname[cu_pwd_len] = DIR_SEPARATOR;
      cuname[cu_pwd_len+1] = 0;
      strcat (cuname, filename);
    }
  else
    /* Filename is an absolute path.  */
    cuname = CONST_CAST (char *, ggc_strdup (filename));

  ctf_add_string (ctfc, cuname, &(ctfc->ctfc_cuname_offset));
  /* Add 1 as CTF strings in the CTF string table are null-terminated
     strings.  */
  ctfc->ctfc_strlen += strlen (cuname) + 1;

  /* Mark cuname for garbage collection.  */
  cuname = NULL;
}

/* Functions to create CTF types.

   These functions perform the task of adding CTF types to the CTF container.
   No de-duplication is done by them; the onus is on the calling function to do
   so.  The caller must first do a lookup via ctf_dtd_lookup or
   ctf_dvd_lookup, as applicable, to ascertain that the CTF type or the CTF
   variable respectively does not already exist, and then add it.  */

static ctf_id_t
ctf_add_generic (ctf_container_ref ctfc, uint32_t flag, const char * name,
		 ctf_dtdef_ref * rp, dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type;

  gcc_assert (flag == CTF_ADD_NONROOT || flag == CTF_ADD_ROOT);

  dtd = ggc_cleared_alloc<ctf_dtdef_t> ();

  type = ctfc->ctfc_nextid++;
  gcc_assert (type < CTF_MAX_TYPE); /* CTF type ID overflow.  */

  /* Buffer the strings in the CTF string table.  */
  dtd->dtd_name = ctf_add_string (ctfc, name, &(dtd->dtd_data.ctti_name));
  dtd->dtd_type = type;
  dtd->dtd_key = die;

  if ((name != NULL) && strcmp (name, ""))
    ctfc->ctfc_strlen += strlen (name) + 1;

  ctf_dtd_insert (ctfc, dtd);

  *rp = dtd;
  return type;
}

static ctf_id_t
ctf_add_encoded (ctf_container_ref ctfc, uint32_t flag, const char * name,
		 const ctf_encoding_t * ep, uint32_t kind, dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type;

  type = ctf_add_generic (ctfc, flag, name, &dtd, die);

  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (kind, flag, 0);

  uint32_t roundup_nbytes = (ROUND_UP (ep->cte_bits, BITS_PER_UNIT)
				    / BITS_PER_UNIT);

  /* FIXME, stay close to what libctf does.  But by getting next power of two,
     aren't we conveying less precise information.  E.g. floating point mode
     XF has a size of 12 bytes.  */
  dtd->dtd_data.ctti_size = roundup_nbytes ? (1 << ceil_log2 (roundup_nbytes))
			   : roundup_nbytes;
  dtd->dtd_u.dtu_enc = *ep;

  ctfc->ctfc_num_stypes++;

  return type;
}

ctf_id_t
ctf_add_reftype (ctf_container_ref ctfc, uint32_t flag, ctf_id_t ref,
		 uint32_t kind, dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type;

  gcc_assert (ref <= CTF_MAX_TYPE);

  type = ctf_add_generic (ctfc, flag, NULL, &dtd, die);
  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (kind, flag, 0);
  /* Caller of this API must guarantee that a CTF type with id = ref already
     exists.  This will also be validated for us at link-time.  */
  dtd->dtd_data.ctti_type = (uint32_t) ref;

  ctfc->ctfc_num_stypes++;

  return type;
}

ctf_id_t
ctf_add_forward (ctf_container_ref ctfc, uint32_t flag, const char * name,
		 uint32_t kind, dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type = 0;

  type = ctf_add_generic (ctfc, flag, name, &dtd, die);

  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (CTF_K_FORWARD, flag, 0);
  dtd->dtd_data.ctti_type = kind;

  ctfc->ctfc_num_stypes++;

  return type;
}

ctf_id_t
ctf_add_typedef (ctf_container_ref ctfc, uint32_t flag, const char * name,
		 ctf_id_t ref, dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type;

  gcc_assert (ref <= CTF_MAX_TYPE);
  /* Nameless Typedefs are not expected.  */
  gcc_assert ((name != NULL) && strcmp (name, ""));

  type = ctf_add_generic (ctfc, flag, name, &dtd, die);
  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (CTF_K_TYPEDEF, flag, 0);
  /* Caller of this API must guarantee that a CTF type with id = ref already
     exists.  This will also be validated for us at link-time.  */
  dtd->dtd_data.ctti_type = (uint32_t) ref;

  gcc_assert (dtd->dtd_type != dtd->dtd_data.ctti_type);

  ctfc->ctfc_num_stypes++;

  return type;
}

ctf_id_t
ctf_add_slice (ctf_container_ref ctfc, uint32_t flag, ctf_id_t ref,
	       uint32_t bit_offset, uint32_t bit_size, dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type;
  uint32_t roundup_nbytes;

  gcc_assert ((bit_size <= 255) && (bit_offset <= 255));

  gcc_assert (ref <= CTF_MAX_TYPE);

  type = ctf_add_generic (ctfc, flag, NULL, &dtd, die);

  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (CTF_K_SLICE, flag, 0);

  roundup_nbytes = (ROUND_UP (bit_size, BITS_PER_UNIT) / BITS_PER_UNIT);
  /* FIXME, stay close to what libctf does.  But by getting next power of two,
     aren't we conveying less precise information, especially for bitfields.
     For example, cte_bits = 33, roundup_nbytes = 5, ctti_size = 8 in the
     implementation below.  */
  dtd->dtd_data.ctti_size = roundup_nbytes ? (1 << ceil_log2 (roundup_nbytes))
					   : 0;

  /* Caller of this API must guarantee that a CTF type with id = ref already
     exists.  This will also be validated for us at link-time.  */
  dtd->dtd_u.dtu_slice.cts_type = (uint32_t) ref;
  dtd->dtd_u.dtu_slice.cts_bits = bit_size;
  dtd->dtd_u.dtu_slice.cts_offset = bit_offset;

  ctfc->ctfc_num_stypes++;

  return type;
}

ctf_id_t
ctf_add_float (ctf_container_ref ctfc, uint32_t flag,
	       const char * name, const ctf_encoding_t * ep, dw_die_ref die)
{
  return (ctf_add_encoded (ctfc, flag, name, ep, CTF_K_FLOAT, die));
}

ctf_id_t
ctf_add_integer (ctf_container_ref ctfc, uint32_t flag,
		 const char * name, const ctf_encoding_t * ep, dw_die_ref die)
{
  return (ctf_add_encoded (ctfc, flag, name, ep, CTF_K_INTEGER, die));
}

ctf_id_t
ctf_add_unknown (ctf_container_ref ctfc, uint32_t flag,
		 const char * name, const ctf_encoding_t * ep, dw_die_ref die)
{
  return (ctf_add_encoded (ctfc, flag, name, ep, CTF_K_UNKNOWN, die));
}

ctf_id_t
ctf_add_pointer (ctf_container_ref ctfc, uint32_t flag, ctf_id_t ref,
		 dw_die_ref die)
{
  return (ctf_add_reftype (ctfc, flag, ref, CTF_K_POINTER, die));
}

ctf_id_t
ctf_add_array (ctf_container_ref ctfc, uint32_t flag, const ctf_arinfo_t * arp,
	       dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type;

  gcc_assert (arp);

  /* Caller of this API must make sure CTF type for arp->ctr_contents and
     arp->ctr_index are already added.  This will also be validated for us at
     link-time.  */

  type = ctf_add_generic (ctfc, flag, NULL, &dtd, die);

  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (CTF_K_ARRAY, flag, 0);
  dtd->dtd_data.ctti_size = 0;
  dtd->dtd_u.dtu_arr = *arp;

  ctfc->ctfc_num_stypes++;

  return type;
}

ctf_id_t
ctf_add_enum (ctf_container_ref ctfc, uint32_t flag, const char * name,
	      HOST_WIDE_INT size, bool eunsigned, dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type;

  /* In the compiler, no need to handle the case of promoting forwards to
     enums.  This comment is simply to note a divergence from libctf.  */

  /* The compiler does, however, update any previously existing forward types
     to non-root.  CTF does not allow existence of two root types with the same
     name.  */
  ctf_dtdef_ref enum_fwd_type = ctf_dtd_lookup (ctfc, die);
  if (enum_fwd_type)
    {
      enum_fwd_type->dtd_data.ctti_info
	= CTF_TYPE_INFO (CTF_K_FORWARD, CTF_ADD_NONROOT, 0);
    }

  type = ctf_add_generic (ctfc, flag, name, &dtd, die);

  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (CTF_K_ENUM, flag, 0);

  /* Size in bytes should always fit, of course.
     TBD WARN - warn instead?  */
  gcc_assert (size <= CTF_MAX_SIZE);

  dtd->dtd_data.ctti_size = size;
  dtd->dtd_enum_unsigned = eunsigned;

  ctfc->ctfc_num_stypes++;

  return type;
}

int
ctf_add_enumerator (ctf_container_ref ctfc, ctf_id_t enid, const char * name,
		    HOST_WIDE_INT value, dw_die_ref die)
{
  ctf_dmdef_t * dmd;
  uint32_t kind, vlen, root;

  /* Callers of this API must make sure that CTF_K_ENUM with enid has been
     addded.  This will also be validated for us at link-time.  */
  ctf_dtdef_ref dtd = ctf_dtd_lookup (ctfc, die);
  gcc_assert (dtd);
  gcc_assert (dtd->dtd_type == enid);
  gcc_assert (name);

  kind = CTF_V2_INFO_KIND (dtd->dtd_data.ctti_info);
  root = CTF_V2_INFO_ISROOT (dtd->dtd_data.ctti_info);
  vlen = CTF_V2_INFO_VLEN (dtd->dtd_data.ctti_info);

  gcc_assert (kind == CTF_K_ENUM && vlen < CTF_MAX_VLEN);

  /* Enum value is of type HOST_WIDE_INT in the compiler, CTF enumerators
     values in ctf_enum_t is limited to int32_t, BTF supports signed and
     unsigned enumerators values of 32 and 64 bits, for both debug formats
     we use ctf_dmdef_t.dmd_value entry of HOST_WIDE_INT type. So check
     CTF bounds and skip adding this enum value if out of bounds.  */
  if (!btf_debuginfo_p() && ((value > INT_MAX) || (value < INT_MIN)))
    {
      /* FIXME - Note this TBD_CTF_REPRESENTATION_LIMIT.  */
      return (1);
    }

  dmd = ggc_cleared_alloc<ctf_dmdef_t> ();

  /* Buffer the strings in the CTF string table.  */
  dmd->dmd_name = ctf_add_string (ctfc, name, &(dmd->dmd_name_offset));
  dmd->dmd_type = CTF_NULL_TYPEID;
  dmd->dmd_offset = 0;

  dmd->dmd_value = value;

  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (kind, root, vlen + 1);
  ctf_dmd_list_append (&dtd->dtd_u.dtu_members, dmd);

  if ((name != NULL) && strcmp (name, ""))
    ctfc->ctfc_strlen += strlen (name) + 1;

  return (0);
}

int
ctf_add_member_offset (ctf_container_ref ctfc, dw_die_ref sou,
		       const char * name, ctf_id_t type,
		       uint64_t bit_offset)
{
  ctf_dtdef_ref dtd = ctf_dtd_lookup (ctfc, sou);
  ctf_dmdef_t * dmd;

  uint32_t kind, vlen, root;

  /* The type of the member being added must already exist.  */
  gcc_assert (dtd);

  kind = CTF_V2_INFO_KIND (dtd->dtd_data.ctti_info);
  root = CTF_V2_INFO_ISROOT (dtd->dtd_data.ctti_info);
  vlen = CTF_V2_INFO_VLEN (dtd->dtd_data.ctti_info);

  gcc_assert (kind == CTF_K_STRUCT || kind == CTF_K_UNION);
  gcc_assert (vlen < CTF_MAX_VLEN);

  dmd = ggc_cleared_alloc<ctf_dmdef_t> ();

  /* Buffer the strings in the CTF string table.  */
  dmd->dmd_name = ctf_add_string (ctfc, name, &(dmd->dmd_name_offset));
  dmd->dmd_type = type;
  dmd->dmd_value = -1;

  if (kind == CTF_K_STRUCT && vlen != 0)
    dmd->dmd_offset = bit_offset;
  else
    dmd->dmd_offset = 0;

  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (kind, root, vlen + 1);
  ctf_dmd_list_append (&dtd->dtd_u.dtu_members, dmd);

  if ((name != NULL) && strcmp (name, ""))
    ctfc->ctfc_strlen += strlen (name) + 1;

  return 0;
}

int
ctf_add_variable (ctf_container_ref ctfc, const char * name, ctf_id_t ref,
		  dw_die_ref die, unsigned int external_vis,
		  dw_die_ref die_var_decl)
{
  ctf_dvdef_ref dvd, dvd_ignore;

  gcc_assert (name);

  if (name != NULL)
    {
      dvd = ggc_cleared_alloc<ctf_dvdef_t> ();
      dvd->dvd_key = die;
      /* Buffer the strings in the CTF string table.  */
      dvd->dvd_name = ctf_add_string (ctfc, name, &(dvd->dvd_name_offset));
      dvd->dvd_visibility = external_vis;
      dvd->dvd_type = ref;

      /* If DW_AT_specification attribute exists, keep track of it as this is
	 the non-defining declaration corresponding to the variable.  We will
	 skip emitting CTF variable for such incomplete, non-defining
	 declarations.
	 There could be some non-defining declarations, however, for which a
	 defining declaration does not show up in the same CU.  For such
	 cases, the compiler continues to emit CTF variable record as
	 usual.  */
      if (die_var_decl)
	{
	  dvd_ignore = ggc_cleared_alloc<ctf_dvdef_t> ();
	  dvd_ignore->dvd_key = die_var_decl;
	  /* It's alright to leave other fields as zero.  No valid CTF
	     variable will be added for these DW_TAG_variable DIEs.  */
	  ctf_dvd_ignore_insert (ctfc, dvd_ignore);
	}

      ctf_dvd_insert (ctfc, dvd);

      if (strcmp (name, ""))
	ctfc->ctfc_strlen += strlen (name) + 1;
    }

  return 0;
}

int
ctf_add_function_arg (ctf_container_ref ctfc, dw_die_ref func,
		      const char * name, ctf_id_t type)
{
  ctf_dtdef_ref dtd = ctf_dtd_lookup (ctfc, func);
  ctf_func_arg_t * farg;
  uint32_t vlen;

  /* The function to which argument is being added must already exist.  */
  gcc_assert (dtd);
  /* The number of args must have been non-zero.  */
  vlen = CTF_V2_INFO_VLEN (dtd->dtd_data.ctti_info);
  gcc_assert (vlen);

  farg = ggc_cleared_alloc<ctf_func_arg_t> ();

  /* Buffer the strings in the auxilliary string table.  CTF V3 format does not
     require function argument names.  Use auxilliary string table to keep
     these strings to avoid unnecessary bloat in CTF section in CTF V3.  */
  farg->farg_name = ctf_add_string (ctfc, name, &(farg->farg_name_offset),
				    CTF_AUX_STRTAB);
  farg->farg_type = type;

  ctf_farg_list_append (&dtd->dtd_u.dtu_argv, farg);

  /* For aux_str, keep ctfc_aux_strlen updated for debugging.  */
  if ((name != NULL) && strcmp (name, ""))
    ctfc->ctfc_aux_strlen += strlen (name) + 1;

  return 0;
}

ctf_id_t
ctf_add_function (ctf_container_ref ctfc, uint32_t flag, const char * name,
		  const ctf_funcinfo_t * ctc, dw_die_ref die,
		  bool from_global_func, int linkage)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type;
  uint32_t vlen;

  gcc_assert (ctc);

  vlen = ctc->ctc_argc;
  gcc_assert (vlen <= CTF_MAX_VLEN);

  type = ctf_add_generic (ctfc, flag, name, &dtd, die);

  dtd->from_global_func = from_global_func;
  dtd->linkage = linkage;
  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (CTF_K_FUNCTION, flag, vlen);
  /* Caller must make sure CTF types for ctc->ctc_return are already added.  */
  dtd->dtd_data.ctti_type = (uint32_t) ctc->ctc_return;
  /* Caller must make sure CTF types for function arguments are already added
     via ctf_add_function_arg () API.  */

  ctfc->ctfc_num_stypes++;

  return type;
}

ctf_id_t
ctf_add_sou (ctf_container_ref ctfc, uint32_t flag, const char * name,
	     uint32_t kind, size_t size, dw_die_ref die)
{
  ctf_dtdef_ref dtd;
  ctf_id_t type = 0;

  gcc_assert ((kind == CTF_K_STRUCT) || (kind == CTF_K_UNION));

  /* In the compiler, no need to handle the case of promoting forwards to
     structs.  This comment is simply to note a divergence from libctf.  */

  /* The compiler does, however, update any previously existing forward types
     to non-root.  CTF does not allow existence of two root types with the same
     name.  */
  ctf_dtdef_ref sou_fwd_type = ctf_dtd_lookup (ctfc, die);
  if (sou_fwd_type)
    {
      sou_fwd_type->dtd_data.ctti_info
	= CTF_TYPE_INFO (CTF_K_FORWARD, CTF_ADD_NONROOT, 0);
    }

  type = ctf_add_generic (ctfc, flag, name, &dtd, die);

  dtd->dtd_data.ctti_info = CTF_TYPE_INFO (kind, flag, 0);

  if (size > CTF_MAX_SIZE)
    {
      dtd->dtd_data.ctti_size = CTF_LSIZE_SENT;
      dtd->dtd_data.ctti_lsizehi = CTF_SIZE_TO_LSIZE_HI (size);
      dtd->dtd_data.ctti_lsizelo = CTF_SIZE_TO_LSIZE_LO (size);
      ctfc->ctfc_num_types++;
    }
  else
    {
      dtd->dtd_data.ctti_size = (uint32_t) size;
      ctfc->ctfc_num_stypes++;
    }

  return type;
}

/* Given a TREE_TYPE node, return the CTF type ID for that type.  */

ctf_id_t
ctf_lookup_tree_type (ctf_container_ref ctfc, const tree type)
{
  dw_die_ref die = lookup_type_die (type);
  if (die == NULL)
    return CTF_NULL_TYPEID;

  ctf_dtdef_ref dtd = ctf_dtd_lookup (ctfc, die);
  if (dtd == NULL)
    return CTF_NULL_TYPEID;

  return dtd->dtd_type;
}

/* Check if CTF for TYPE has already been generated.  Mainstay for
   de-duplication.  If CTF type already exists, returns TRUE and updates
   the TYPE_ID for the caller.  */

bool
ctf_type_exists (ctf_container_ref ctfc, dw_die_ref type,
		 ctf_id_t * type_id)
{
  bool exists = false;
  ctf_dtdef_ref ctf_type_seen = ctf_dtd_lookup (ctfc, type);

  if (ctf_type_seen)
    {
      exists = true;
      /* CTF type for this type exists.  */
      *type_id = ctf_type_seen->dtd_type;
    }

  return exists;
}

/* Location information for CTF Types and CTF Variables.  CTF section does not
   emit location information; at this time, location information is needed for
   BTF CO-RE use-cases.  */

int
ctfc_get_dtd_srcloc (ctf_dtdef_ref dtd, ctf_srcloc_ref loc)
{
  loc->ctsloc_file = ctf_get_die_loc_file (dtd->dtd_key);
  loc->ctsloc_line = ctf_get_die_loc_line (dtd->dtd_key);
  loc->ctsloc_col = ctf_get_die_loc_col (dtd->dtd_key);

  if (loc->ctsloc_file == NULL)
    return 1;

  return 0;
}

int
ctfc_get_dvd_srcloc (ctf_dvdef_ref dvd, ctf_srcloc_ref loc)
{
  loc->ctsloc_file = ctf_get_die_loc_file (dvd->dvd_key);
  loc->ctsloc_line = ctf_get_die_loc_line (dvd->dvd_key);
  loc->ctsloc_col = ctf_get_die_loc_col (dvd->dvd_key);

  if (loc->ctsloc_file == NULL)
    return 1;

  return 0;
}

/* CTF container setup and teardown routines.  */

/* Initialize the CTF string table.
   The first entry in the CTF string table (empty string) is added.  */

static void
init_ctf_strtable (ctf_strtable_t * strtab)
{
  strtab->ctstab_head = NULL;
  strtab->ctstab_tail = NULL;
  strtab->ctstab_num = 0;
  strtab->ctstab_len = 0;

  /* The first entry in the CTF string table is an empty string.  E.g., CTF
     type records with no name (like CTF_K_CONST, CTF_K_VOLATILE etc) point to
     this string.  */
  uint32_t estr_offset = 0;
  strtab->ctstab_estr = ctfc_strtable_add_str (strtab, "", &estr_offset);
}

/* Initialize the string tables in the CTF container.  */

static void
init_ctf_string_table (ctf_container_ref ctfc)
{
  init_ctf_strtable (&ctfc->ctfc_strtable);
  ctfc->ctfc_strlen++;

  init_ctf_strtable (&ctfc->ctfc_aux_strtable);
  ctfc->ctfc_aux_strlen++;
}

/* Allocate a new CTF container with the desired flags.  */

static inline ctf_container_ref
new_ctf_container (void)
{
  tu_ctfc = ggc_cleared_alloc<ctf_container_t> ();
  tu_ctfc->ctfc_types
    = hash_table<ctfc_dtd_hasher>::create_ggc (100);
  tu_ctfc->ctfc_vars
    = hash_table<ctfc_dvd_hasher>::create_ggc (100);
  tu_ctfc->ctfc_ignore_vars
    = hash_table<ctfc_dvd_hasher>::create_ggc (10);

  return tu_ctfc;
}

/* Initialize a CTF container per translation unit.  */

static void
init_ctf_container (void)
{
  tu_ctfc = new_ctf_container ();

  tu_ctfc->ctfc_magic = CTF_MAGIC;
  tu_ctfc->ctfc_version = CTF_VERSION;
  tu_ctfc->ctfc_flags = CTF_F_NEWFUNCINFO;
  tu_ctfc->ctfc_nextid = CTF_INIT_TYPEID;

  init_ctf_string_table (tu_ctfc);
}

void
ctfc_delete_strtab (ctf_strtable_t * strtab)
{
  ctf_string_t * str = NULL;
  ctf_string_t * next_str = NULL;

  str = strtab->ctstab_head;
  next_str = str;
  while (next_str != NULL)
    {
      next_str = str->cts_next;
      ggc_free (str);
      str = next_str;
    }

  strtab->ctstab_head = NULL;
  strtab->ctstab_tail = NULL;
  strtab->ctstab_estr = NULL;
}

/* Delete the CTF container's resources.  */

void
ctfc_delete_container (ctf_container_ref ctfc)
{
  if (ctfc)
    {
      ctfc->ctfc_types->empty ();
      ctfc->ctfc_types = NULL;

      ctfc->ctfc_vars->empty ();
      ctfc->ctfc_types = NULL;

      ctfc->ctfc_ignore_vars->empty ();
      ctfc->ctfc_ignore_vars = NULL;

      ctfc_delete_strtab (&ctfc->ctfc_strtable);
      ctfc_delete_strtab (&ctfc->ctfc_aux_strtable);
      if (ctfc->ctfc_vars_list)
	{
	  ggc_free (ctfc->ctfc_vars_list);
	  ctfc->ctfc_vars_list = NULL;
	}
      if (ctfc->ctfc_types_list)
	{
	  ggc_free (ctfc->ctfc_types_list);
	  ctfc->ctfc_types_list = NULL;
	}
      if (ctfc->ctfc_gfuncs_list)
	{
	  ggc_free (ctfc->ctfc_gfuncs_list);
	  ctfc->ctfc_gfuncs_list = NULL;
	}
      if (ctfc->ctfc_gobjts_list)
	{
	  ggc_free (ctfc->ctfc_gobjts_list);
	  ctfc->ctfc_gobjts_list = NULL;
	}

      ctfc= NULL;
    }
}

/* CTF routines interfacing to the compiler.  */

void
ctf_init (void)
{
  init_ctf_container ();
}
