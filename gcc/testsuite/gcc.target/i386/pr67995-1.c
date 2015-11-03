/* { dg-do compile } */
/* { dg-options "-O2 -march=haswell" } */

unsigned int
__attribute__ ((target("arch=core2")))
__x86_rdrand(void)
{
  unsigned int retries = 100;
  unsigned int val;

  while (__builtin_ia32_rdrand32_step(&val) == 0) /* { dg-error "needs isa option" } */
    if (--retries == 0)
      return 0;

  return val;
}
