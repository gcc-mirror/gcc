/* Configuration file for an HP 320.
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Control assembler-syntax conditionals in m68k.md.  */

#ifndef USE_GAS
#define MOTOROLA		/* Use Motorola syntax rather than "MIT" */
#define SGS			/* Uses SGS assembler */
#define SGS_CMP_ORDER		/* Takes cmp operands in reverse order */
#define HPUX_ASM

#if !defined (CROSS_COMPILE) && !defined (NO_BUGS)
/* The assembler on HP 9k3xx machines running HPUX 8.0 doesn't translate
   floating point constants behind some operands.  The workaround is to
   use hex constants.  Reported by Thomas Nau (nau@medizin.uni-ulm.de).  */
#define AS_BUG_FLOATING_CONSTANT
/* The assembler on HP 9k3xx machines running HPUX 8.0 doesn't accept
   labels followed by a text, data, or other section directive.  Reported
   by Thomas Nau (nau@medizin.uni-ulm.de).  */
#define AS_BUG_TRAILING_LABEL
#endif

#endif /* not USE_GAS */
