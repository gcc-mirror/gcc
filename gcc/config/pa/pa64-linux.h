/* Definitions for PA_RISC with ELF format on 64-bit Linux
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

#undef CPP_SPEC
#define CPP_SPEC "%{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{mhppa:-D__hppa__} %{posix:-D_POSIX_SOURCE} -D_PA_RISC2_0 -D__LP64__"

#if 0 /* needs some work :-( */
/* If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  */

#define ELIMINABLE_REGS							\
{									\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},				\
  {ARG_POINTER_REGNUM,	 FRAME_POINTER_REGNUM},				\
}

/* A C expression that returns non-zero if the compiler is allowed to try to
   replace register number FROM with register number TO.  The frame pointer
   is automatically handled.  */

#define CAN_ELIMINATE(FROM, TO) 1

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  do								\
    {								\
      int fsize;						\
								\
      fsize = compute_frame_size (get_frame_size (), 0);	\
      if ((TO) == FRAME_POINTER_REGNUM				\
	  && (FROM) == ARG_POINTER_REGNUM)			\
	{							\
	  (OFFSET) = -16;					\
	  break;						\
	}							\
								\
      if ((TO) != STACK_POINTER_REGNUM)				\
	abort ();						\
								\
      switch (FROM)						\
	{							\
	case FRAME_POINTER_REGNUM:				\
	  (OFFSET) = - fsize;					\
	  break;						\
								\
	case ARG_POINTER_REGNUM:				\
	  (OFFSET) = - fsize - 16;				\
	  break;						\
								\
	default:						\
	  abort ();						\
	}							\
    } while (0)
#endif
