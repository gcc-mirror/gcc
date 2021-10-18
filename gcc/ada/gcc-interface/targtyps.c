/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            T A R G T Y P S                               *
 *                                                                          *
 *                                  Body                                    *
 *                                                                          *
 *          Copyright (C) 1992-2021, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed  with GNAT;  see file  COPYING3.  If not see *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* Functions for retrieving target types.  See Ada package Get_Targ.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "target.h"
#include "tree.h"

#include "ada.h"
#include "types.h"
#include "sinfo.h"
#include "ada-tree.h"
#include "gigi.h"

/* If we don't have a specific size for Ada's equivalent of `long', use that
   of C.  */
#ifndef ADA_LONG_TYPE_SIZE
#define ADA_LONG_TYPE_SIZE LONG_TYPE_SIZE
#endif

/* The following provide a functional interface for the front end Ada code
   to determine the sizes that are used for various C types. */

Pos
get_target_bits_per_unit (void)
{
  return BITS_PER_UNIT;
}

Pos
get_target_bits_per_word (void)
{
  return BITS_PER_WORD;
}

Pos
get_target_char_size (void)
{
  return CHAR_TYPE_SIZE;
}

Pos
get_target_wchar_t_size (void)
{
  /* We never want wide characters less than "short" in Ada.  */
  return MAX (SHORT_TYPE_SIZE, WCHAR_TYPE_SIZE);
}

Pos
get_target_short_size (void)
{
  return SHORT_TYPE_SIZE;
}

Pos
get_target_int_size (void)
{
  return INT_TYPE_SIZE;
}

Pos
get_target_long_size (void)
{
  return ADA_LONG_TYPE_SIZE;
}

Pos
get_target_long_long_size (void)
{
  return LONG_LONG_TYPE_SIZE;
}

Pos
get_target_long_long_long_size (void)
{
  if (targetm.scalar_mode_supported_p (TImode))
    return GET_MODE_BITSIZE (TImode);
  else
    return LONG_LONG_TYPE_SIZE;
}

Pos
get_target_pointer_size (void)
{
  return POINTER_SIZE;
}

/* Alignment related values, mapped to attributes for functional and
   documentation purposes.  */

/* Standard'Maximum_Default_Alignment.  Maximum alignment that the compiler
   might choose by default for a type or object.

   Stricter alignment requests trigger gigi's aligning_type circuitry for
   stack objects or objects allocated by the default allocator.  */

Pos
get_target_maximum_default_alignment (void)
{
  return BIGGEST_ALIGNMENT / BITS_PER_UNIT;
}

/* Standard'System_Allocator_Alignment.  Alignment guaranteed to be honored
   by the default allocator (System.Memory.Alloc or malloc if we have no
   run-time library at hand).

   Stricter alignment requests trigger gigi's aligning_type circuitry for
   objects allocated by the default allocator.  */

/* ??? Need a way to get info about __gnat_malloc from here (whether it is
   handy and what alignment it honors).  In the meantime, resort to malloc
   considerations only.  */

/* Account for MALLOC_OBSERVABLE_ALIGNMENTs here.  Use this or the ABI
   guaranteed alignment if greater.  */

#ifdef MALLOC_OBSERVABLE_ALIGNMENT
#define MALLOC_ALIGNMENT MALLOC_OBSERVABLE_ALIGNMENT
#else
#define MALLOC_OBSERVABLE_ALIGNMENT (2 * POINTER_SIZE)
#define MALLOC_ALIGNMENT \
  MAX (MALLOC_ABI_ALIGNMENT, MALLOC_OBSERVABLE_ALIGNMENT)
#endif

Pos
get_target_system_allocator_alignment (void)
{
  return MALLOC_ALIGNMENT / BITS_PER_UNIT;
}

/* Standard'Maximum_Allowed_Alignment.  Maximum alignment that we may
   accept for any type or object.  */

#ifndef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT BIGGEST_ALIGNMENT
#endif

Pos
get_target_maximum_allowed_alignment (void)
{
  return MAX_OFILE_ALIGNMENT / BITS_PER_UNIT;
}

/* Standard'Maximum_Alignment.  The single attribute initially made
   available, now a synonym of Standard'Maximum_Default_Alignment.  */

Pos
get_target_maximum_alignment (void)
{
  return get_target_maximum_default_alignment ();
}

#ifndef FLOAT_WORDS_BIG_ENDIAN
#define FLOAT_WORDS_BIG_ENDIAN WORDS_BIG_ENDIAN
#endif

Nat
get_target_float_words_be (void)
{
  return FLOAT_WORDS_BIG_ENDIAN;
}

Nat
get_target_words_be (void)
{
  return WORDS_BIG_ENDIAN;
}

Nat
get_target_bytes_be (void)
{
  return BYTES_BIG_ENDIAN;
}

Nat
get_target_bits_be (void)
{
  return BITS_BIG_ENDIAN;
}

Nat
get_target_strict_alignment (void)
{
  return STRICT_ALIGNMENT;
}

Nat
get_target_double_float_alignment (void)
{
#ifdef TARGET_ALIGN_NATURAL
  /* This macro is only defined by the rs6000 port.  */
  if (!TARGET_ALIGN_NATURAL
      && (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_DARWIN))
    return 32 / BITS_PER_UNIT;
#endif
  return 0;
}

Nat
get_target_double_scalar_alignment (void)
{
#ifdef TARGET_ALIGN_DOUBLE
  /* This macro is only defined by the i386 and sh ports.  */
  if (!TARGET_ALIGN_DOUBLE
#ifdef TARGET_64BIT
      && !TARGET_64BIT
#endif
     )
    return 32 / BITS_PER_UNIT;
#endif
  return 0;
}
