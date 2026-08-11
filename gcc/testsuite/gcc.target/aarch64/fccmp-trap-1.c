/* { dg-do run } */
/* { dg-options "-O2" } */

enum { FPSR_IOC = 1 << 0 };

__attribute__ ((noipa))
static int
and_lt (double a, double b, double c, double d)
{
  return (a < b) & (c < d);
}

int
main (void)
{
  double qnan = __builtin_nan ("");

  __builtin_aarch64_set_fpsr (0);
  if (and_lt (1.0, 0.0, qnan, 0.0) != 0)
    __builtin_abort ();
  if ((__builtin_aarch64_get_fpsr () & FPSR_IOC) == 0)
    __builtin_abort ();

  return 0;
}
