/* OSF/rose half-pic support definitions.
   Copyright (C) 1992 Free Software Foundation, Inc.

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

extern int  flag_half_pic;		/* Global half-pic flag.  */
extern int  half_pic_number_ptrs;	/* # distinct pointers found */
extern int  half_pic_number_refs;	/* # half-pic references */
extern void half_pic_encode ();		/* encode whether half-pic */
extern void half_pic_declare ();	/* declare object local */
extern void half_pic_init ();		/* half_pic initialization */
extern void half_pic_finish ();		/* half_pic termination */
extern int  half_pic_address_p ();	/* true if an address is half-pic */
extern struct rtx_def *half_pic_ptr ();	/* return RTX for half-pic pointer */

/* Macros to provide access to the half-pic stuff (so they can easily
   be stubbed out.  */

#define HALF_PIC_P()		(flag_half_pic)
#define HALF_PIC_NUMBER_PTRS	(half_pic_number_ptrs)
#define HALF_PIC_NUMBER_REFS	(half_pic_number_refs)

#define HALF_PIC_ENCODE(DECL)	half_pic_encode (DECL)
#define HALF_PIC_DECLARE(NAME)	half_pic_declare (NAME)
#define HALF_PIC_INIT()		half_pic_init ()
#define HALF_PIC_FINISH(STREAM)	half_pic_finish (STREAM)
#define HALF_PIC_ADDRESS_P(X)	half_pic_address_p (X)
#define HALF_PIC_PTR(X)		half_pic_ptr (X)

/* Prefix for half-pic names */
#ifndef HALF_PIC_PREFIX
#define	HALF_PIC_PREFIX	"__pic_"
#endif
