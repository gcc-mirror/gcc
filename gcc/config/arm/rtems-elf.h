/* Definitions for RTEMS based ARM systems using ELF
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
 
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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Run-time Target Specification.  */
#undef  TARGET_VERSION
#define TARGET_VERSION  fputs (" (ARM/ELF RTEMS)", stderr);

#define HAS_INIT_SECTION

#undef  CPP_PREDEFINES
#define CPP_PREDEFINES "-Darm -Darm_elf -Drtems -D__rtems__ -D__ELF__ \
   -Asystem(rtems) -Acpu(arm) -Amachine(arm)"

/*#undef INVOKE_main*/


