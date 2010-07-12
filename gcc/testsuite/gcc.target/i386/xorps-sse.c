/* Test that we generate xorps instruction when pxor is not available.  */
/* { dg-do compile } */
/* { dg-options "-O -msse -mno-sse2" } */
/* { dg-require-effective-target sse } */
/* { dg-final { scan-assembler "xorps\[ \t\]" } } */

#define vector __attribute__ ((vector_size (16)))

vector int i(vector int f)
{
  vector int g = { 0x80000000, 0, 0x80000000, 0 };
  vector int f_int = (vector int) f;
  return (f_int ^ g);
}

