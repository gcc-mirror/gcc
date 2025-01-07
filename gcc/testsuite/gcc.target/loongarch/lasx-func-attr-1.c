/* { dg-do compile } */
/* { dg-options "-O2 -mno-lsx" } */

typedef int v8i32 __attribute__ ((vector_size(32), aligned(32)));
extern v8i32 a, b, c;

#ifndef TEST_TARGET_PRAGMA
__attribute__ ((target ("lasx")))
#else
#pragma GCC target ("lasx")
#endif
void
test (void)
{
  a = b + c;
}


/* { dg-final { scan-assembler "xvadd.w" } } */
