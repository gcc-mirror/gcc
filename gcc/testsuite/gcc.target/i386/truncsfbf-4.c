/* { dg-do compile } */
/* { dg-options "-msse2 -O2 -ffast-math -mno-avxneconvert -mavx512vl -mavx512bf16 -fno-asynchronous-unwind-tables" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "ia32" "*ia32" "" { target ia32 } {^\t?\.} } } */
/* { dg-final { check-function-bodies "x64" "*x64" "" { target { ! ia32 } } {^\t?\.} } } */

/*
ia32foo:
ia32	vmovss	4\(%esp\), %xmm0
ia32	vcvtneps2bf16	%xmm0, %xmm0
ia32	ret
ia32...
*ia32

x64foo:
x64	vcvtneps2bf16	%xmm0, %xmm0
x64	ret
x64...
*x64
*/

__bf16
foo (float a)
{
  return a;
}
