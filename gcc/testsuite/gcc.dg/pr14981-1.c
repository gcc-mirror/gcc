/* PR target/14981  */
/* PR target/20051  */
/* Test case reduced by Ferdinand <commie1@gmx.net> */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse" } */
typedef float v4sf __attribute__ ((vector_size (16)));

void foo(float* y)
{
  v4sf x = __builtin_ia32_xorps (x,x);
  __builtin_ia32_storeaps (y, x);
}
