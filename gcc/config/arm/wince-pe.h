/* Definitions of target machine for GNU compiler, for ARM with WINCE-PE obj format.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Nick Clifton <nickc@redhat.com>
   
   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* Override arm/pe.h's default apcs26 support.  */

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT	(ARM_FLAG_APCS_32 | ARM_FLAG_SOFT_FLOAT | TARGET_FLAG_NOP_FUN | ARM_FLAG_MMU_TRAPS)

#undef  MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
  { "marm", "mlittle-endian", "msoft-float", "mapcs-32", "mno-thumb-interwork" }  
