/* Expand a SYMBOL into its corresponding dllimport, far-address,
or refptr symbol.
Copyright (C) 1988-2025 Free Software Foundation, Inc.

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
http://www.gnu.org/licenses/.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "stringpool.h"
#include "emit-rtl.h"
#include "alias.h"
#include "varasm.h"
#include "output.h"
#include "explow.h"
#include "winnt.h"

/* Create or return the unique __imp_DECL dllimport symbol corresponding
   to symbol DECL if BEIMPORT is true.  Otherwise create or return the
   unique refptr-DECL symbol corresponding to symbol DECL.  */

struct dllimport_hasher : ggc_cache_ptr_hash<tree_map>
{
  static inline hashval_t hash (tree_map *m)
  {
    return m->hash;
  }

  static inline bool
  equal (tree_map *a, tree_map *b)
  {
    return a->base.from == b->base.from;
  }

  static int
  keep_cache_entry (tree_map *&m)
  {
    return ggc_marked_p (m->base.from);
  }
};

static GTY ((cache)) hash_table<dllimport_hasher> *dllimport_map;

/*  Nonzero if the symbol is marked as dllimport, or as stub-variable,
    otherwise zero.  */

bool
is_imported_p (rtx x)
{
  if (!TARGET_DLLIMPORT_DECL_ATTRIBUTES
      || GET_CODE (x) != SYMBOL_REF)
    return false;

  return SYMBOL_REF_DLLIMPORT_P (x) || SYMBOL_REF_STUBVAR_P (x);
}

/* Return a unique alias set for the GOT.  */

alias_set_type
mingw_GOT_alias_set (void)
{
  static alias_set_type set = -1;
  if (set == -1)
    set = new_alias_set ();
  return set;
}

static tree
get_dllimport_decl (tree decl, bool beimport)
{
  struct tree_map *h, in;
  const char *name;
  const char *prefix;
  size_t namelen, prefixlen;
  char *imp_name;
  tree to;
  rtx rtl;

  if (!dllimport_map)
    dllimport_map = hash_table<dllimport_hasher>::create_ggc (512);

  in.hash = htab_hash_pointer (decl);
  in.base.from = decl;
  tree_map **loc = dllimport_map->find_slot_with_hash (&in, in.hash, INSERT);
  h = *loc;
  if (h)
    return h->to;

  *loc = h = ggc_alloc<tree_map> ();
  h->hash = in.hash;
  h->base.from = decl;
  h->to = to = build_decl (DECL_SOURCE_LOCATION (decl),
			   VAR_DECL, NULL, ptr_type_node);
  DECL_ARTIFICIAL (to) = 1;
  DECL_IGNORED_P (to) = 1;
  DECL_EXTERNAL (to) = 1;
  TREE_READONLY (to) = 1;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  name = targetm.strip_name_encoding (name);
  if (beimport)
    prefix = name[0] == FASTCALL_PREFIX || user_label_prefix[0] == 0
      ? "*__imp_" : "*__imp__";
  else
    prefix = user_label_prefix[0] == 0 ? "*.refptr." : "*refptr.";
  namelen = strlen (name);
  prefixlen = strlen (prefix);
  imp_name = (char *) alloca (namelen + prefixlen + 1);
  memcpy (imp_name, prefix, prefixlen);
  memcpy (imp_name + prefixlen, name, namelen + 1);

  name = ggc_alloc_string (imp_name, namelen + prefixlen);
  rtl = gen_rtx_SYMBOL_REF (Pmode, name);
  SET_SYMBOL_REF_DECL (rtl, to);
  SYMBOL_REF_FLAGS (rtl) = SYMBOL_FLAG_LOCAL | SYMBOL_FLAG_STUBVAR;
  if (!beimport)
    {
      SYMBOL_REF_FLAGS (rtl) |= SYMBOL_FLAG_EXTERNAL;
#ifdef SUB_TARGET_RECORD_STUB
      SUB_TARGET_RECORD_STUB (name, decl);
#endif
    }

  rtl = gen_const_mem (Pmode, rtl);
  set_mem_alias_set (rtl, GOT_ALIAS_SET);

  SET_DECL_RTL (to, rtl);
  SET_DECL_ASSEMBLER_NAME (to, get_identifier (name));

  return to;
}

/* Expand SYMBOL into its corresponding far-address symbol.
   WANT_REG is true if we require the result be a register.  */

static rtx
legitimize_pe_coff_extern_decl (rtx symbol, bool want_reg)
{
  tree imp_decl;
  rtx x;

  gcc_assert (SYMBOL_REF_DECL (symbol));
  imp_decl = get_dllimport_decl (SYMBOL_REF_DECL (symbol), false);

  x = DECL_RTL (imp_decl);
  if (want_reg)
    x = force_reg (Pmode, x);
  return x;
}

/* Expand SYMBOL into its corresponding dllimport symbol.  WANT_REG is
   true if we require the result be a register.  */

static rtx
legitimize_dllimport_symbol (rtx symbol, bool want_reg)
{
  tree imp_decl;
  rtx x;

  gcc_assert (SYMBOL_REF_DECL (symbol));
  imp_decl = get_dllimport_decl (SYMBOL_REF_DECL (symbol), true);

  x = DECL_RTL (imp_decl);
  if (want_reg)
    x = force_reg (Pmode, x);
  return x;
}

/* Expand SYMBOL into its corresponding dllimport or refptr symbol.  WANT_REG
   is true if we require the result be a register.  */

rtx
legitimize_pe_coff_symbol (rtx addr, bool inreg)
{
  if (!TARGET_PECOFF)
    return NULL_RTX;

  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES)
    {
      if (GET_CODE (addr) == SYMBOL_REF && SYMBOL_REF_DLLIMPORT_P (addr))
	return legitimize_dllimport_symbol (addr, inreg);
      if (GET_CODE (addr) == CONST
	  && GET_CODE (XEXP (addr, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (addr, 0), 0)) == SYMBOL_REF
	  && SYMBOL_REF_DLLIMPORT_P (XEXP (XEXP (addr, 0), 0)))
	{
	  rtx t = legitimize_dllimport_symbol (XEXP (XEXP (addr, 0), 0), inreg);
	  return gen_rtx_PLUS (Pmode, t, XEXP (XEXP (addr, 0), 1));
	}
    }

  if (!PE_COFF_LEGITIMIZE_EXTERN_DECL (addr))
    return NULL_RTX;

  if (GET_CODE (addr) == SYMBOL_REF
      && !is_imported_p (addr)
      && SYMBOL_REF_EXTERNAL_P (addr)
      && SYMBOL_REF_DECL (addr))
    return legitimize_pe_coff_extern_decl (addr, inreg);

  if (GET_CODE (addr) == CONST
      && GET_CODE (XEXP (addr, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (addr, 0), 0)) == SYMBOL_REF
      && !is_imported_p (XEXP (XEXP (addr, 0), 0))
      && SYMBOL_REF_EXTERNAL_P (XEXP (XEXP (addr, 0), 0))
      && SYMBOL_REF_DECL (XEXP (XEXP (addr, 0), 0)))
    {
      rtx t = legitimize_pe_coff_extern_decl (XEXP (XEXP (addr, 0), 0), inreg);
      return gen_rtx_PLUS (Pmode, t, XEXP (XEXP (addr, 0), 1));
    }
  return NULL_RTX;
}

#include "gt-winnt-dll.h"
