/* PR target/126231  */
/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

#include <emmintrin.h>

/*
**func1:
**.LFB[0-9]+:
**	.cfi_startproc
**	cvttss2siq	%xmm0, %rax
**	movl	%eax, %eax
**	ret
**	.cfi_endproc
**...
*/

unsigned long long
func1 (float x)
{
  return (unsigned int) x;
}

/*
**func2:
**.LFB[0-9]+:
**	.cfi_startproc
**	cvttsd2siq	%xmm0, %rax
**	movl	%eax, %eax
**	ret
**	.cfi_endproc
**...
*/

unsigned long long
func2 (double x)
{
  return (unsigned int) x;
}

/*
**func3:
**.LFB[0-9]+:
**	.cfi_startproc
**	cvttss2sil	%xmm0, %eax
**	ret
**	.cfi_endproc
**...
*/

unsigned long long
func3 (__m128 x)
{
  return (unsigned int) _mm_cvttss_si32 (x);
}

/*
**func4:
**.LFB[0-9]+:
**	.cfi_startproc
**	cvtss2sil	%xmm0, %eax
**	ret
**	.cfi_endproc
**...
*/

unsigned long long
func4 (__m128 x)
{
  return (unsigned int) _mm_cvtss_si32 (x);
}

/*
**func5:
**.LFB[0-9]+:
**	.cfi_startproc
**	cvttsd2sil	%xmm0, %eax
**	ret
**	.cfi_endproc
**...
*/

unsigned long long
func5 (__m128d x)
{
  return (unsigned int) _mm_cvttsd_si32 (x);
}

/*
**func6:
**.LFB[0-9]+:
**	.cfi_startproc
**	cvtsd2sil	%xmm0, %eax
**	ret
**	.cfi_endproc
**...
*/

unsigned long long
func6 (__m128d x)
{
  return (unsigned int) _mm_cvtsd_si32 (x);
}
