/* { dg-do compile } */
/* { dg-options "-O1 -msimd=lasx" } */

typedef long long v4i64 __attribute__ ((vector_size (32), aligned (32)));
extern long long *x_si;
v4i64
test (void)
{
  v4i64 a = { x_si[1], x_si[0], 0x411, 0x411 };
  return a;
}
