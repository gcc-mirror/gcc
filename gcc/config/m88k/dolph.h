/* Definitions of target machine for GNU compiler.
   Motorola m88100 running the Dolphin UNIX System V/88 Release 3.2,
   Version 3.8/7.83 and 3.6/5.86
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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

#include "m88k/sysv3.h"

#define SDB_ALLOW_FORWARD_REFERENCES
#define SDB_ALLOW_UNKNOWN_REFERENCES

/* Override m88k/sysv3.h */

#undef	CPP_PREDEFINES
#define CPP_PREDEFINES "-Dm88000 -Dm88k -DOCS88 -DDOLPHIN -Dunix -DsysV88 -D__CLASSIFY_TYPE__=2 -Asystem(unix) -Asystem(svr3) -Acpu(m88k) -Amachine(m88k)" 

/* 
  If you want to detect dereferencing of NULL pointers, uncomment the
  following two lines. Alternatively, edit the appropriate specs file.
  
  #undef LINK_SPEC
  #define LINK_SPEC "gcc.ld%s"
  
  */

#undef CPU_DEFAULT
#define CPU_DEFAULT MASK_88000

#undef INITIALIZE_TRAMPOLINE 
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 40)), FNADDR); \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 36)), CXT); \
  emit_call_insn (gen_call( gen_rtx (MEM, SImode,			\
				     gen_rtx(SYMBOL_REF,Pmode,		\
					     "__enable_execute_stack")), \
			   const0_rtx));				\
}
