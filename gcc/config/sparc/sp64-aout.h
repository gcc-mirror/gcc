/* Definitions of target machine for GNU compiler,
   for Sun SPARC-V9 on a hypothetical a.out format machine.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Doug Evans, dje@cygnus.com.

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

/* This is a v9 only compiler.  -mv8 is not expected to work.  If you want
   a v8/v9 compiler, this isn't the place to do it.  */
/* ??? Until real v9 machines exist, all of this is subject to change.  */
/* ??? This file should really be called sp64-sunos4.h or some such but that
   would be a bit misleading since no such machines exist yet.  The current
   name is also misleading since the term "aout" is more properly applied to
   embedded configurations.  */

#define SPARCV9 /* See sparc.h.  */

#include "sparc/sparc.h"

/* A v9 compiler with 32 bit integers and 64 bit pointers,
   in a Medium/Low code model with only 32 bit assembler support.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_MEDLOW + MASK_ENV32 + MASK_PTR64 + MASK_HARD_QUAD + MASK_EPILOGUE + MASK_FPU)

/* ??? Disabled for v9 as the current implementation of the Medium/Anywhere
   code model needs this in the data segment (still true?).  Let's hope the
   assembler is fixed.  */
#undef JUMP_TABLES_IN_TEXT_SECTION

/* Put all data in the data segment (necessary for the current implementation
   of the Medium/Anywhere code model - see if still true).  */

#define READONLY_DATA_SECTION data_section
