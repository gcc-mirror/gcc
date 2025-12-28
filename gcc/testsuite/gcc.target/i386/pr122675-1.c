/* Verify that the most likely BB edges are optimized as fallthroughs.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-pic -march=x86-64 -mtune=generic -mgeneral-regs-only -fomit-frame-pointer" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**test:
**.LFB[0-9]+:
**	.cfi_startproc
**	testl	%edi, %edi
**	je	.L[0-9]*
**	subq	\$[0-9]*, %rsp
**	.cfi_def_cfa_offset [0-9]*
**	call	f1
**	addq	\$[0-9]*, %rsp
**	.cfi_def_cfa_offset [0-9]*
**	ret
**.L[0-9]+:
**	movl	%edi, %eax
**	ret
**...
*/

int f1(void);

int test(int a)
{
  if (__builtin_expect(!!a, 1)) {
    return f1();
  }
  return a;
}
