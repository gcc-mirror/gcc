/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mmemcpy-strategy=vector_loop:2048:noalign,libcall:-1:noalign" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	xorl	%eax, %eax
**.L[0-9]+:
**	movl	%eax, %edx
**	addl	\$256, %eax
**	vmovdqa64	src\(%rdx\), %zmm3
**	vmovdqa64	src\+64\(%rdx\), %zmm2
**	vmovdqa64	src\+128\(%rdx\), %zmm1
**	vmovdqa64	src\+192\(%rdx\), %zmm0
**	vmovdqa64	%zmm3, dest\(%rdx\)
**	vmovdqa64	%zmm2, dest\+64\(%rdx\)
**	vmovdqa64	%zmm1, dest\+128\(%rdx\)
**	vmovdqa64	%zmm0, dest\+192\(%rdx\)
**	cmpl	\$1024, %eax
**	jb	.L[0-9]+
**	vmovdqa	src\(%rax\), %ymm0
**	vmovdqa	%ymm0, dest\(%rax\)
**	vmovdqu	src\+31\(%rax\), %ymm0
**	vmovdqu	%ymm0, dest\+31\(%rax\)
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
