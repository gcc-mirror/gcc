/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O -m32 -g -mno-sse" } */

__attribute__((target_clones("default,sse2")))
void a()
{
  __attribute__((__vector_size__(4 * sizeof(float)))) int b = {};
}
