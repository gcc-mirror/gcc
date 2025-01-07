/* { dg-do compile } */
/* { dg-options "-O2 -mno-lsx" } */

typedef int v4i32 __attribute__ ((vector_size(16), aligned(16)));
extern v4i32 a, b, c;

__attribute__ ((target ("lsx")))
void
test (void)
{
  a = b + c;
}


/* { dg-final { scan-assembler "vadd.w" } } */
