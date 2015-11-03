/* { dg-do compile } */
/* { dg-options "-O2 -march=core2" } */

unsigned int
__attribute__ ((target("arch=haswell")))
__x86_rdrand(void)
{
  unsigned int retries = 100;
  unsigned int val;

  while (__builtin_ia32_rdrand32_step(&val) == 0)
    if (--retries == 0)
      return 0;

  return val;
}
