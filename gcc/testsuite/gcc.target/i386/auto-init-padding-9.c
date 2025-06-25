/* Verify zero initialization for array type with structure element with
   padding.  */ 
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**...
**	pxor	%xmm0, %xmm0
**...
**.L[0-9]+:
**	movl	%esi, %ecx
**	movaps	%xmm0, \(%rdx,%rcx\)
**	movaps	%xmm0, 16\(%rdx,%rcx\)
**	movaps	%xmm0, 32\(%rdx,%rcx\)
**	movaps	%xmm0, 48\(%rdx,%rcx\)
**	addl	\$64, %esi
**	cmpl	%edi, %esi
**	jb	.L[0-9]+
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
