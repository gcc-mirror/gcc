/* { dg-do compile } */
/* { dg-options "-O2 -mno-lsx" } */

typedef int v4i32 __attribute__ ((vector_size(16), aligned(16)));
extern v4i32 a, b, c;

#ifndef TEST_TARGET_PRAGMA
__attribute__ ((target ("lsx")))
#else
#pragma GCC target ("lsx")
#endif
void
test (void)
{
  a = b + c;
}


/* { dg-final { scan-assembler "vadd.w" } } */
