/* { dg-do compile } */
/* { dg-options "-O2 -mno-lsx" } */

typedef int v8i32 __attribute__ ((vector_size(32), aligned(32)));
extern v8i32 a, b, c;

__attribute__ ((target ("lasx")))
void
test (void)
{
  a = b + c;
}


/* { dg-final { scan-assembler "xvadd.w" } } */
