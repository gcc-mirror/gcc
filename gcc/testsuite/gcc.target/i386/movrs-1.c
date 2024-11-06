/* { dg-do compile } */
/* { dg-options "-mmovrs -O2" } */
/* { dg-final { scan-assembler-times "movrsb\[ \\t\]\+\\(%(?:r|e).x\\), %.l" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movrsw\[ \\t\]\+\\(%(?:r|e).x\\), %.x" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movrsl\[ \\t\]\+\\(%(?:r|e).x\\), %e.x" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movrsq\[ \\t\]\+\\(%(?:r|e).x\\), %r.x" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "prefetchrst2\[ \\t\]" 1 } } */


#include <immintrin.h>

volatile char x1;
volatile short x2;
volatile int x3;
volatile long long x4;
char * p1;
short * p2;
int * p3;
long long * p4;


void extern
movrs_test (void)
{
  _m_prefetchrs (p1);
#ifdef __x86_64__
  x1 = _movrs_i8 (p1);
  x2 = _movrs_i16 (p2);
  x3 = _movrs_i32 (p3);
  x4 = _movrs_i64 (p4);
#endif
}
