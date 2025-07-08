/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemcpy-strategy=vector_loop:2048:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	xorl	%edx, %edx
**.L[0-9]+:
**	movl	%edx, %eax
**	subl	\$-128, %edx
**	vmovdqa	src\(%rax\), %ymm3
**	vmovdqa	src\+32\(%rax\), %ymm2
**	vmovdqa	src\+64\(%rax\), %ymm1
**	vmovdqa	src\+96\(%rax\), %ymm0
**	vmovdqa	%ymm3, dest\(%rax\)
**	vmovdqa	%ymm2, dest\+32\(%rax\)
**	vmovdqa	%ymm1, dest\+64\(%rax\)
**	vmovdqa	%ymm0, dest\+96\(%rax\)
**	cmpl	\$512, %edx
**	jb	.L[0-9]+
**	vmovdqa	src\(%rdx\), %ymm0
**	vmovdqa	%ymm0, dest\(%rdx\)
**	vmovdqu	src\+31\(%rdx\), %ymm0
**	vmovdqu	%ymm0, dest\+31\(%rdx\)
**	vzeroupper
**	ret
**...
*/

#define SIZE 16 * 32 + 32 + 31

char dest[SIZE];
char src[SIZE];

void
foo (void)
{
  __builtin_memcpy (dest, src, SIZE);
}

/* { dg-final { scan-assembler-not "rep mov" } } */
