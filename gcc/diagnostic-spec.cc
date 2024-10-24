/* Functions to enable and disable individual warnings on an expression
   and statement basis.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by Martin Sebor <msebor@redhat.com>

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "bitmap.h"
#include "tree.h"
#include "cgraph.h"
#include "hash-map.h"
#include "diagnostic-spec.h"
#include "pretty-print.h"
#include "options.h"

/* Initialize *THIS from warning option OPT.  */

nowarn_spec_t::nowarn_spec_t (opt_code opt)
{
  /* Create a very simple mapping based on testing and experience.
     It should become more refined with time. */
  switch (opt)
    {
    case no_warning:
      m_bits = 0;
      break;

    case all_warnings:
      m_bits = -1;
      break;

      /* Flow-sensitive warnings about pointer problems issued by both
	 front ends and the middle end.  */
    case OPT_Waddress:
    case OPT_Wnonnull:
      m_bits = NW_NONNULL;
      break;

      /* Flow-sensitive warnings about arithmetic overflow issued by both
	 front ends and the middle end.  */
    case OPT_Woverflow:
    case OPT_Wshift_count_negative:
    case OPT_Wshift_count_overflow:
    case OPT_Wstrict_overflow:
      m_bits = NW_VFLOW;
      break;

      /* Lexical warnings issued by front ends.  */
    case OPT_Wabi:
    case OPT_Wlogical_op:
    case OPT_Wparentheses:
    case OPT_Wreturn_type:
    case OPT_Wsizeof_array_div:
    case OPT_Wstrict_aliasing:
    case OPT_Wunused:
    case OPT_Wunused_function:
    case OPT_Wunused_but_set_variable:
    case OPT_Wunused_variable:
    case OPT_Wunused_but_set_parameter:
      m_bits = NW_LEXICAL;
      break;

      /* Access warning group.  */
    case OPT_Warray_bounds_:
    case OPT_Wformat_overflow_:
    case OPT_Wformat_truncation_:
    case OPT_Wrestrict:
    case OPT_Wsizeof_pointer_memaccess:
    case OPT_Wstrict_aliasing_:
    case OPT_Wstringop_overflow_:
    case OPT_Wstringop_overread:
    case OPT_Wstringop_truncation:
      m_bits = NW_ACCESS;
      break;

      /* Initialization warning group.  */
    case OPT_Winit_self:
    case OPT_Wuninitialized:
    case OPT_Wmaybe_uninitialized:
      m_bits = NW_UNINIT;
      break;

    case OPT_Wdangling_pointer_:
    case OPT_Wreturn_local_addr:
    case OPT_Wuse_after_free_:
      m_bits = NW_DANGLING;
      break;

    case OPT_Wpessimizing_move:
    case OPT_Wredundant_move:
      m_bits = NW_REDUNDANT;
      break;

    default:
      /* A catchall group for everything else.  */
      m_bits = NW_OTHER;
    }
}

/* A mapping from a 'location_t' to the warning spec set for it.  */

GTY(()) nowarn_map_t *nowarn_map;

/* Return the no-warning disposition for location LOC and option OPT
   or for all/any otions by default.  */

bool
warning_suppressed_at (location_t loc, opt_code opt /* = all_warnings */)
{
  gcc_checking_assert (!RESERVED_LOCATION_P (loc));

  if (!nowarn_map)
    return false;

  if (const nowarn_spec_t* const pspec = nowarn_map->get (loc))
    {
      const nowarn_spec_t optspec (opt);
      return *pspec & optspec;
    }

  return false;
}

 /* Change the supression of warnings for location LOC.
    OPT controls which warnings are affected.
    The wildcard OPT of -1 controls all warnings.
    If SUPP is true (the default), enable the suppression of the warnings.
    If SUPP is false, disable the suppression of the warnings.  */

bool
suppress_warning_at (location_t loc, opt_code opt /* = all_warnings */,
		     bool supp /* = true */)
{
  gcc_checking_assert (!RESERVED_LOCATION_P (loc));

  const nowarn_spec_t optspec (supp ? opt : opt_code ());

  if (nowarn_spec_t *pspec = nowarn_map ? nowarn_map->get (loc) : NULL)
    {
      if (supp)
	{
	  *pspec |= optspec;
	  return true;
	}

      *pspec &= optspec;
      if (*pspec)
	return true;

      nowarn_map->remove (loc);
      return false;
    }

  if (!supp || opt == no_warning)
    return false;

  if (!nowarn_map)
    nowarn_map = nowarn_map_t::create_ggc (32);

  nowarn_map->put (loc, optspec);
  return true;
}

/* Change the warning disposition for LOC to match OPTSPEC.  */

void
put_warning_spec_at (location_t loc, unsigned bits)
{
  gcc_checking_assert (!RESERVED_LOCATION_P (loc));

  nowarn_spec_t optspec = nowarn_spec_t::from_bits (bits);
  if (!optspec)
    {
      if (nowarn_map)
	nowarn_map->remove (loc);
    }
  else
    {
      if (!nowarn_map)
	nowarn_map = nowarn_map_t::create_ggc (32);
      nowarn_map->put (loc, optspec);
    }
}

/* Copy the no-warning disposition from one location to another.  */

void
copy_warning (location_t to, location_t from)
{
  if (!nowarn_map)
    return;

  nowarn_spec_t *from_spec;
  if (RESERVED_LOCATION_P (from))
    from_spec = NULL;
  else
    from_spec = nowarn_map->get (from);
  if (RESERVED_LOCATION_P (to))
    /* We cannot set no-warning dispositions for 'to', so we have no chance but
       lose those potentially set for 'from'.  */
    ;
  else
    {
      if (from_spec)
	{
	  nowarn_spec_t tem = *from_spec;
	  nowarn_map->put (to, tem);
	}
      else
	nowarn_map->remove (to);
    }
}
