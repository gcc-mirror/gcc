/* Definitions of target machine for GNU compiler,
   targetting the XSCALE/VxWorks/ELF run time environment. 
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

#undef TARGET_VERSION
#define TARGET_VERSION	fputs (" (XScale/ELF VxWorks)", stderr);

#define SUBTARGET_EXTRA_ASM_SPEC "%{!mcpu=*:-mxscale} %{!mhard-float:-mno-fpu}"

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
  { "mlittle-endian", "mno-thumb-interwork", "marm", "msoft-float" }
#endif

#undef FLOAT_WORDS_BIG_ENDIAN 
#define FLOAT_WORDS_BIG_ENDIAN (TARGET_BIG_END != 0)
