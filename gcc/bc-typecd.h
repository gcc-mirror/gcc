/* Typecode definitions for Bytecode Interpreter.
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef TYPECODE_H
#define TYPECODE_H

enum typecode
{
#define DEFTYPECODE(CODE, NAME, MACHMODE, TYPE) CODE,
#include "bc-typecd.def"
#undef DEFTYPECODE

  LAST_AND_UNUSED_TYPECODE
};

/* Determine if a given type is integer.  */
#define TYPECODE_INTEGER_P(TYPECODE) ((int) (TYPECODE) < (int) SFcode)

/* Determine if a given type is unsigned.  */
#define TYPECODE_UNSIGNED_P(TYPECODE) \
  (TYPECODE_INTEGER_P(TYPECODE) && (int) (TYPECODE) & 1)

/* Determine if a given type is signed.  */
#define TYPECODE_SIGNED_P(TYPECODE) \
  (TYPECODE_INTEGER_P(TYPECODE) && !((int) (TYPECODE) & 1))

/* Determine if a given type is floating.  */
#define TYPECODE_FLOAT_P(TYPECODE) \
  ((int) (TYPECODE) < (int) Pcode && !TYPECODE_INTEGER_P(TYPECODE))

/* Determine if the given type is arithmetic. */
#define TYPECODE_ARITH_P(TYPECODE) \
  (TYPECODE_INTEGER_P(TYPECODE) || TYPECODE_FLOAT_P(TYPECODE))

#define NUM_TYPECODES ((int) LAST_AND_UNUSED_TYPECODE)

#endif
