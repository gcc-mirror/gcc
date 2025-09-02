/* { dg-do compile { target { *-*-linux* && ia32 } } } */
/* { dg-options "-O2 -fno-pic -mtune=generic -msse2 -mno-avx -mno-mmx -mno-80387 -mtune-ctrl=prologue_using_move,epilogue_using_move" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/* end must be empty.  */

/*
**end:
**.LFB[0-9]+:
**	.cfi_startproc
**	ret
**	.cfi_endproc
**...
*/

/* inc doesn't have any callee saved registers.  */

/*
**inc:
**.LFB[0-9]+:
**	.cfi_startproc
**	addl	\$1, accumulator
**	movl	4\(%esp\), %eax
**	leal	4\(%eax\), %edx
**	movl	%edx, 4\(%esp\)
**	jmp	\*\(%eax\)
**	.cfi_endproc
**...
*/

/* dec doesn't have any callee saved registers.  */

/*
**dec:
**.LFB[0-9]+:
**	.cfi_startproc
**	subl	\$1, accumulator
**	movl	4\(%esp\), %eax
**	leal	4\(%eax\), %edx
**	movl	%edx, 4\(%esp\)
**	jmp	\*\(%eax\)
**	.cfi_endproc
**...
*/

/* start must save and restore all caller saved registers.  */

/*
**start:
**.LFB[0-9]+:
**	.cfi_startproc
**...
**	movl	%eax, 144\(%esp\)
**	movl	%edx, 148\(%esp\)
**	movl	%ecx, 152\(%esp\)
**	movl	%ebx, 156\(%esp\)
**	movl	%esi, 160\(%esp\)
**	movl	%edi, 164\(%esp\)
**	movaps	%xmm0, 12\(%esp\)
**	movaps	%xmm1, 28\(%esp\)
**	movaps	%xmm2, 44\(%esp\)
**	movaps	%xmm3, 60\(%esp\)
**	movaps	%xmm4, 76\(%esp\)
**	movaps	%xmm5, 92\(%esp\)
**	movaps	%xmm6, 108\(%esp\)
**	movaps	%xmm7, 124\(%esp\)
**...
**	pushl	\$code\+4
**...
**	call	\*code
**	movaps	16\(%esp\), %xmm0
**	movaps	32\(%esp\), %xmm1
**	movaps	48\(%esp\), %xmm2
**	movaps	64\(%esp\), %xmm3
**	movaps	80\(%esp\), %xmm4
**	movaps	96\(%esp\), %xmm5
**	movaps	112\(%esp\), %xmm6
**	movaps	128\(%esp\), %xmm7
**	movl	148\(%esp\), %eax
**	movl	152\(%esp\), %edx
**	movl	156\(%esp\), %ecx
**	movl	160\(%esp\), %ebx
**	movl	164\(%esp\), %esi
**	movl	168\(%esp\), %edi
**...
**	ret
**	.cfi_endproc
**...
*/

#include "no-callee-saved-19a.c"
