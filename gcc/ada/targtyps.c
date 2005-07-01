/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            T A R G T Y P S                               *
 *                                                                          *
 *                                  Body                                    *
 *                                                                          *
 *          Copyright (C) 1992-2004 Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* Functions for retrieving target types. See Ada package Get_Targ */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "real.h"
#include "rtl.h"
#include "ada.h"
#include "types.h"
#include "atree.h"
#include "elists.h"
#include "namet.h"
#include "nlists.h"
#include "snames.h"
#include "stringt.h"
#include "uintp.h"
#include "urealp.h"
#include "fe.h"
#include "sinfo.h"
#include "einfo.h"
#include "ada-tree.h"
#include "gigi.h"

/* If we don't have a specific size for Ada's equivalent of `long', use that
   of C.  */
#ifndef ADA_LONG_TYPE_SIZE
#define ADA_LONG_TYPE_SIZE LONG_TYPE_SIZE
#endif

#ifndef WIDEST_HARDWARE_FP_SIZE
#define WIDEST_HARDWARE_FP_SIZE LONG_DOUBLE_TYPE_SIZE
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
  /* We never want wide chacters less than "short" in Ada.  */
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
get_target_float_size (void)
{
  return fp_prec_to_size (FLOAT_TYPE_SIZE);
}

Pos
get_target_double_size (void)
{
  return fp_prec_to_size (DOUBLE_TYPE_SIZE);
}

Pos
get_target_long_double_size (void)
{
  return fp_prec_to_size (WIDEST_HARDWARE_FP_SIZE);
}


Pos
get_target_pointer_size (void)
{
  return POINTER_SIZE;
}

Pos
get_target_maximum_alignment (void)
{
  return BIGGEST_ALIGNMENT / BITS_PER_UNIT;
}

#ifndef FLOAT_WORDS_BIG_ENDIAN
#define FLOAT_WORDS_BIG_ENDIAN WORDS_BIG_ENDIAN
#endif

Nat
get_float_words_be (void)
{
  return FLOAT_WORDS_BIG_ENDIAN;
}

Nat
get_words_be (void)
{
  return WORDS_BIG_ENDIAN;
}

Nat
get_bytes_be (void)
{
  return BYTES_BIG_ENDIAN;
}

Nat
get_bits_be (void)
{
  return BITS_BIG_ENDIAN;
}

Nat
get_strict_alignment (void)
{
  return STRICT_ALIGNMENT;
}
