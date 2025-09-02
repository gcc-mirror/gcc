/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -march=x86-64-v4 -fpic -fplt -mtls-dialect=gnu" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	vpbroadcastb	%edi, %zmm0
**...
**	call	__tls_get_addr@PLT
**...
*/

#include <immintrin.h>

extern __m512i sinkz;
extern __m256i sinky;
extern __m128i sinkx;
extern void func1 (long *);
extern int func2 (void);
extern void func3 (void);
static __thread long var;

long
foo (char c)
{
  func1 (&var);
  if (func2 ())
    func3 ();
  sinkx = _mm_set1_epi8 (c);
  sinkz = _mm512_set1_epi8 (c);
  sinky = _mm256_set1_epi8 (c);
  return var;
}

/* { dg-final { scan-assembler-times "vpbroadcastb" 1 } } */
/* { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } } */
