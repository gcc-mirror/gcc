/* Definitions of target machine for GNU compiler
   for an Intel i386 or later processor running OS/2 2.x.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Contributed by Samuel Figueroa (figueroa@cs.nyu.edu)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef DEFAULT_TARGET_MACHINE
#define DEFAULT_TARGET_MACHINE "i386-os2"
#endif
#ifndef LINK_SPEC
#define LINK_SPEC "/st:1048576/pm:vio/noi/a:16/e/bas:65536/nol"
#endif
#ifndef LIB_SPEC
#define LIB_SPEC "libc"
#endif
#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC ""
#endif
#ifndef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "\\gcc\\bin\\"
#endif
#ifndef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "\\gcc\\lib\\"
#endif
#ifndef LOCAL_INCLUDE_DIR
#define LOCAL_INCLUDE_DIR "\\gcc\\include"
#endif

#define YES_UNDERSCORES
#include "i386/gstabs.h"

#define USE_COLLECT

#define BIGGEST_FIELD_ALIGNMENT \
  (maximum_field_alignment ? maximum_field_alignment : 32)

extern int maximum_field_alignment;

#undef PCC_BITFIELD_TYPE_MATTERS
#define PCC_BITFIELD_TYPE_MATTERS (maximum_field_alignment == 0)

/* Define this macro if it is advisible to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT         \
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
      (MODE) = SImode;

/* Define this if function arguments should also be promoted using the above
   procedure.  */

#define PROMOTE_FUNCTION_ARGS

/* Likewise, if the function return value is promoted.  */

#define PROMOTE_FUNCTION_RETURN
