/* Test that we generate xorps when the result is used in FP math.  */
/* { dg-do compile } */
/* { dg-options "-O -msse2 -mno-sse3" } */
/* { dg-final { scan-assembler "xorps\[ \t\]" } } */
/* { dg-final { scan-assembler-not "pxor" } } */

#define vector __attribute__ ((vector_size (16)))

vector float i(vector float f, vector float h)
{
  vector int g = { 0x80000000, 0, 0x80000000, 0 };
  vector int f_int = (vector int) f;
  return ((vector float) (f_int ^ g)) + h;
}

