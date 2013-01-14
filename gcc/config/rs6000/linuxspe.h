/* Definitions of target machine for GNU compiler,
   for PowerPC e500 machines running GNU/Linux.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez (aldy@quesejoda.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Override rs6000.h and sysv4.h definition.  */
#undef	TARGET_DEFAULT
#define TARGET_DEFAULT MASK_STRICT_ALIGN

#undef  ASM_DEFAULT_SPEC
#define	ASM_DEFAULT_SPEC "-mppc -mspe -me500"
