/* More startup code for A/UX */

#include "tm.h"

#ifdef USE_BIN_AS
	file "crt2.s"

/* The init section is used to support shared libraries */
	init
	global	__istart

__istart:
	link %fp,&-4
#else
	.file "crt2.s"

/* The init section is used to support shared libraries */
.section .init, "x"
.even
.globl __istart

__istart:
	link %fp,#-4

#ifndef USE_COLLECT2
/* The ctors and dtors sections are used to support COFF collection of 
   c++ constructors and destructors */
.section .ctors, "d"
.even
.globl __CTOR_LIST__

__CTOR_LIST__:
	.long -1

.section .dtors, "d"
.even
.globl __DTOR_LIST__

__DTOR_LIST__:
	.long -1
#endif /* USE_COLLECT2 */
#endif /* USE_BIN_AS */
