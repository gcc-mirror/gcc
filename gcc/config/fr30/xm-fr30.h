/* Definitions of FR30 target.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

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

/* A C expression for the status code to be returned when the compiler exits
   after serious errors.  */
#define FATAL_EXIT_CODE 33

/* A C expression for the status code to be returned when the compiler exits
   without serious errors.  */
#define SUCCESS_EXIT_CODE 0

/* Defined if the host machine stores words of multi-word values in big-endian
   order.  (GNU CC does not depend on the host byte ordering within a word.)  */
/* #define HOST_WORDS_BIG_ENDIAN 1 */

/* In addition, configuration files for system V define `bcopy', `bzero' and
   `bcmp' as aliases.  Some files define `alloca' as a macro when compiled with
   GNU CC, in order to take advantage of the benefit of GNU CC's built-in
   `alloca'.  */

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"

/* end of xm-fr30.h */
