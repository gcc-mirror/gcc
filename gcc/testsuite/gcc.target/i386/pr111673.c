/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue -fasynchronous-unwind-tables -fdwarf2-cfi-asm" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**advance:
**.LFB0:
**	.cfi_startproc
**	testl	%edi, %edi
**	jle	.L2
**	imull	%edi, %edi
**	leal	\(%rdi,%rdi\), %eax
**	ret
**...
*/

/* Verify there is an early return without the prolog and shrink-wrap
   the function. */

int f (int);
int
advance (int dz)
{
  if (dz > 0)
    return (dz + dz) * dz;
  else
    return dz * f (dz);
}

/* { dg-final { scan-rtl-dump-times "Performing shrink-wrapping" 1 "pro_and_epilogue" } } */
