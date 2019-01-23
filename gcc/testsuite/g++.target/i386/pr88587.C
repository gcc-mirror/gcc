/* { dg-do compile { target ia32 } } */
/* { dg-require-ifunc "" }  */
/* { dg-options "-O -fno-pic -g -mno-sse -Wno-attributes" } */

__attribute__((target("default"),always_inline))
void a()
{
  __attribute__((__vector_size__(4 * sizeof(float)))) int b = {};
}

__attribute__((target("sse2"))) void a2()
{
  a ();
  __attribute__((__vector_size__(4 * sizeof(float)))) int b = {};
}
