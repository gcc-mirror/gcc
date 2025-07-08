/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemcpy-strategy=vector_loop:2048:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	xorl	%edx, %edx
**.L[0-9]+:
**	movl	%edx, %eax
**	addl	\$256, %edx
**	vmovdqa64	src\(%rax\), %zmm3
**	vmovdqa64	src\+64\(%rax\), %zmm2
**	vmovdqa64	src\+128\(%rax\), %zmm1
**	vmovdqa64	src\+192\(%rax\), %zmm0
**	vmovdqa64	%zmm3, dest\(%rax\)
**	vmovdqa64	%zmm2, dest\+64\(%rax\)
**	vmovdqa64	%zmm1, dest\+128\(%rax\)
**	vmovdqa64	%zmm0, dest\+192\(%rax\)
**	cmpl	\$1024, %edx
**	jb	.L[0-9]+
**	vmovdqa	src\(%rdx\), %ymm0
**	vmovdqa	%ymm0, dest\(%rdx\)
**	vmovdqu	src\+31\(%rdx\), %ymm0
**	vmovdqu	%ymm0, dest\+31\(%rdx\)
**	vzeroupper
**	ret
**...
*/

#define SIZE 16 * 64 + 63

char dest[SIZE] __attribute__((aligned(64)));
char src[SIZE] __attribute__((aligned(64)));

void
foo (void)
{
  __builtin_memcpy (dest, src, SIZE);
}

/* { dg-final { scan-assembler-not "rep mov" } } */
