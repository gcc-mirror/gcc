/* { dg-do compile { target { *-*-linux* && maybe_x32 } } } */
/* { dg-options "-O2 -mx32 -fno-pic -mtune=generic -msse2 -mno-apxf -mtune-ctrl=prologue_using_move,epilogue_using_move" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB[0-9]+:
**	.cfi_startproc
**	subl	\$8, %esp
**	.cfi_def_cfa_offset 16
**	call	boring
**	movl	continuation\(%rip\), %eax
**	addl	\$8, %esp
**	.cfi_def_cfa_offset 8
**	jmp	\*%rax
**	.cfi_endproc
**...
*/

#include "preserve-none-30a.c"
