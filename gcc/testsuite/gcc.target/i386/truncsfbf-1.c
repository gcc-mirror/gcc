/* { dg-do compile } */
/* { dg-options "-msse2 -O2 -ffast-math -mno-avxneconvert -mno-avx512bf16" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "ia32" "*ia32" "" { target ia32 } {^\t?\.} } } */
/* { dg-final { check-function-bodies "x64" "*x64" "" { target { ! ia32 } } {^\t?\.} } } */

/*
ia32foo:
ia32.LFB0:
ia32	.cfi_startproc
ia32	movl	4\(%esp\), %edx
ia32	movl	%edx, %eax
ia32	shrl	\$16, %eax
ia32	movl	%eax, %ecx
ia32	andl	\$1, %eax
ia32	leal	32767\(%edx,%eax\), %eax
ia32	andl	\$32768, %ecx
ia32	shrl	\$16, %eax
ia32	andl	\$2139095040, %edx
ia32	cmove	%ecx, %eax
ia32	(v|)movd	%eax, %xmm0
ia32	ret
ia32...
*ia32

x64foo:
x64.LFB0:
x64	.cfi_startproc
x64	(v|)movd	%xmm0, %edx
x64	movl	%edx, %eax
x64	shrl	\$16, %eax
x64	movl	%eax, %ecx
x64	andl	\$1, %eax
x64	leal	32767\(%rdx,%rax\), %eax
x64	andl	\$32768, %ecx
x64	shrl	\$16, %eax
x64	andl	\$2139095040, %edx
x64	cmove	%ecx, %eax
x64	(v|)movd	%eax, %xmm0
x64	ret
x64...
*x64
*/

__bf16
foo (float a)
{
  return a;
}
