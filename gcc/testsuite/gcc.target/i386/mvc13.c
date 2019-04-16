/* { dg-do compile { target ia32 } } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O -g -mno-sse" } */

__attribute__((target_clones("default,sse2")))
void a()
{
  __attribute__((__vector_size__(4 * sizeof(float)))) int b = {};
}
