/* OSF/rose half-pic support definitions.
   Copyright (C) 1992, 1996, 1997, 1998 Free Software Foundation, Inc.

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

#ifndef NO_HALF_PIC

#ifdef ANSI_PROTOTYPES
union tree_node;		/* forward reference */
struct rtx_def;
#endif

/* Declare the variable flag_half_pic as 'int' instead of 'extern
   int', so that BSS variables are created (even though this is not
   strict ANSI).  This is because rtl.c now refers to the
   CONSTANT_ADDRESS_P macro, which in turn refers to flag_half_pic,
   and wants to call half_pic_address_p, whose address we also store
   in a BSS variable.  This way, the gen* programs won't get
   unknown symbol errors when being linked (flag_half_pic will never
   be true in the gen* programs).  */

int flag_half_pic;			/* Global half-pic flag.  */
int (*ptr_half_pic_address_p) ();	/* ptr to half_pic_address_p () */

extern int  half_pic_number_ptrs;				/* # distinct pointers found */
extern int  half_pic_number_refs;				/* # half-pic references */
extern void half_pic_encode PROTO((union tree_node *));		/* encode whether half-pic */
extern void half_pic_declare PROTO((char *));			/* declare object local */
extern void half_pic_external PROTO((char *));			/* declare object external */
extern void half_pic_init PROTO((void));			/* half_pic initialization */
extern int  half_pic_address_p PROTO((struct rtx_def *));	/* true if an address is half-pic */
extern struct rtx_def *half_pic_ptr PROTO((struct rtx_def *));	/* return RTX for half-pic pointer */
/* Can't use prototype since FILE isn't defined yet.  */
extern void half_pic_finish (/* FILE * */);		/* half_pic termination */

/* Macros to provide access to the half-pic stuff (so they can easily
   be stubbed out.  */

#define HALF_PIC_P()		(flag_half_pic)
#define HALF_PIC_NUMBER_PTRS	(half_pic_number_ptrs)
#define HALF_PIC_NUMBER_REFS	(half_pic_number_refs)

#define HALF_PIC_ENCODE(DECL)	half_pic_encode (DECL)
#define HALF_PIC_DECLARE(NAME)	half_pic_declare (NAME)
#define HALF_PIC_EXTERNAL(NAME)	half_pic_external (NAME)
#define HALF_PIC_INIT()		half_pic_init ()
#define HALF_PIC_FINISH(STREAM)	half_pic_finish (STREAM)
#define HALF_PIC_ADDRESS_P(X)	((*ptr_half_pic_address_p) (X))
#define HALF_PIC_PTR(X)		half_pic_ptr (X)

/* Prefix for half-pic names */
#ifndef HALF_PIC_PREFIX
#define	HALF_PIC_PREFIX	"__pic_"
#endif

#endif /* NO_HALF_PIC */
