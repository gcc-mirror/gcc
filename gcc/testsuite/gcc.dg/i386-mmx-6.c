/* PR middle-end/17767 */
/* Contributed by Volker Reichelt <reichelt@igpm.rwth-aachen.de> */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -mmmx" } */
typedef int __m64 __attribute__ ((vector_size (8)));
typedef short __v4hi __attribute__ ((vector_size (8)));

__m64 foo ()
{
  int i;
  __m64 m;

  for (i = 0; i < 2; i++)
    m = (__m64) __builtin_ia32_pcmpeqw ((__v4hi) m, (__v4hi) m);

  return m;
}
