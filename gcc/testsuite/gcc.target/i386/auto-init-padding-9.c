/* Verify zero initialization for array type with structure element with
   padding.  */ 
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**...
**	leaq	-160\(%rbp\), %rax
**	movq	%rax, %rcx
**	pxor	%xmm0, %xmm0
**	movl	\$160, %edx
**	movl	%edx, %edi
**	andl	\$-64, %edi
**	movl	\$0, %esi
**.L[0-9]+:
**	movl	%esi, %edx
**	movaps	%xmm0, \(%rax,%rdx\)
**	movaps	%xmm0, 16\(%rax,%rdx\)
**	movaps	%xmm0, 32\(%rax,%rdx\)
**	movaps	%xmm0, 48\(%rax,%rdx\)
**	addl	\$64, %esi
**	cmpl	%edi, %esi
**	jb	.L[0-9]+
**	movl	%esi, %eax
**	addq	%rax, %rcx
**	movaps	%xmm0, \(%rcx\)
**	movaps	%xmm0, 16\(%rcx\)
**	movzbl	-116\(%rbp\), %eax
**	movsbl	%al, %eax
**...
*/

struct test_trailing_hole {
        int one;
        int two;
        int three;
        char four;
        /* "sizeof(unsigned long) - 1" byte padding hole here. */
};


int foo ()
{
  struct test_trailing_hole var[10]; 
  return var[2].four;
}

/* { dg-final { scan-assembler-not "rep stos" } } */
