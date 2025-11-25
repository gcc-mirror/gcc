/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64-v2 -std=gnu++17" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**_Z4TestPc:
**.LFB[0-9]+:
**	.cfi_startproc
**	movzbl	-1\(%rdi\), %eax
**	pxor	%xmm1, %xmm1
**	movd	%eax, %xmm0
**	pshufb	%xmm1, %xmm0
**	movups	%xmm0, \(%rdi\)
**	movups	%xmm0, 16\(%rdi\)
**	movups	%xmm0, 32\(%rdi\)
**	movups	%xmm0, 48\(%rdi\)
**	ret
**...
*/

#include <x86intrin.h>

void 
Test (char* dst)
{
  __m128i pattern = _mm_set1_epi8(dst[-1]);
  for (int i = 0; i < 4; i++)
    _mm_storeu_si128(reinterpret_cast<__m128i*>(dst + 16 * i), pattern);
}
