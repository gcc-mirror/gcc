/* Definitions for "naked" Intel 386 using a.out (or coff encap'd
   a.out) object format and stabs debugging info.

   Copyright (C) 1994 Free Software Foundation, Inc.

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


/* This is tested by gas.h.  */
#define YES_UNDERSCORES

#include "i386/gstabs.h"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Di386"

/* end of i386-aout.h */
