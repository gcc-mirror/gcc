/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mmemcpy-strategy=vector_loop:2048:noalign,libcall:-1:noalign" } */
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
**	vmovdqa64	src\(%rdx\), %zmm0
**	vmovdqa64	%zmm0, dest\(%rdx\)
**	vzeroupper
**	ret
**...
*/

#define SIZE (16 + 1) * 64

char dest[SIZE] __attribute__((aligned(64)));
char src[SIZE] __attribute__((aligned(64)));

void
foo (void)
{
  __builtin_memcpy (dest, src, SIZE);
}

/* { dg-final { scan-assembler-not "rep mov" } } */
