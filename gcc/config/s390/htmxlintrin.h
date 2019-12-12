/* XL compiler hardware transactional execution intrinsics
   Copyright (C) 2013-2019 Free Software Foundation, Inc.
   Contributed by Andreas Krebbel (Andreas.Krebbel@de.ibm.com)

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

#ifndef _HTMXLINTRIN_H
#define _HTMXLINTRIN_H

#include <stdint.h>

#include <htmintrin.h>

#ifdef __cplusplus
extern "C" {
#endif

/* These intrinsics are being made available for compatibility with
   the IBM XL compiler.  For documentation please see the "z/OS XL
   C/C++ Programming Guide" publicly available on the web.  */

/* FIXME: __TM_simple_begin and __TM_begin should be marked
   __always_inline__ as well but this currently produces an error
   since the tbegin builtins are "returns_twice" and setjmp_call_p
   (calls.c) therefore identifies the functions as calling setjmp.
   The tree inliner currently refuses to inline functions calling
   setjmp.  */

long
__TM_simple_begin ()
{
  return __builtin_tbegin_nofloat (0);
}

long
__TM_begin (void* const tdb)
{
  return __builtin_tbegin_nofloat (tdb);
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_end ()
{
  return __builtin_tend ();
}

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_abort ()
{
  return __builtin_tabort (_HTM_FIRST_USER_ABORT_CODE);
}

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_named_abort (unsigned char const code)
{
  return __builtin_tabort ((int)_HTM_FIRST_USER_ABORT_CODE + code);
}

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_non_transactional_store (void* const addr, long long const value)
{
  __builtin_non_tx_store ((uint64_t*)addr, (uint64_t)value);
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_nesting_depth (void* const tdb_ptr)
{
  int depth = __builtin_tx_nesting_depth ();
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;

  if (depth != 0)
    return depth;

  if (tdb->format != 1)
    return 0;
  return tdb->nesting_depth;
}

/* Transaction failure diagnostics */

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_user_abort (void* const tdb_ptr)
{
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;

  if (tdb->format != 1)
    return 0;

  return !!(tdb->abort_code >= _HTM_FIRST_USER_ABORT_CODE);
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_named_user_abort (void* const tdb_ptr, unsigned char* code)
{
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;

  if (tdb->format != 1)
    return 0;

  if (tdb->abort_code >= _HTM_FIRST_USER_ABORT_CODE)
    {
      *code = tdb->abort_code - _HTM_FIRST_USER_ABORT_CODE;
      return 1;
    }
  return 0;
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_illegal (void* const tdb_ptr)
{
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;

  return (tdb->format == 1
	  && (tdb->abort_code == 4 /* unfiltered program interruption */
	      || tdb->abort_code == 11 /* restricted instruction */));
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_footprint_exceeded (void* const tdb_ptr)
{
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;

  return (tdb->format == 1
	  && (tdb->abort_code == 7 /* fetch overflow */
	      || tdb->abort_code == 8 /* store overflow */));
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_nested_too_deep (void* const tdb_ptr)
{
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;

  return tdb->format == 1 && tdb->abort_code == 13; /* depth exceeded */
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_conflict (void* const tdb_ptr)
{
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;

  return (tdb->format == 1
	  && (tdb->abort_code == 9 /* fetch conflict */
	      || tdb->abort_code == 10 /* store conflict */));
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_is_failure_persistent (long const result)
{
  return result == _HTM_TBEGIN_PERSISTENT;
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_failure_address (void* const tdb_ptr)
{
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;
#ifdef __s390x__
  return tdb->atia;
#else
  return tdb->atia & 0xffffffff;
#endif
}

extern __inline long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__TM_failure_code (void* const tdb_ptr)
{
  struct __htm_tdb *tdb = (struct __htm_tdb*)tdb_ptr;

  return tdb->abort_code;
}

#ifdef __cplusplus
}
#endif

#endif /* _HTMXLINTRIN_H */
