/* { dg-do run { target rv64 } } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64d" } */

__attribute__ ((noipa)) long
f (unsigned long y)
{
  unsigned int x = (unsigned int) y;
  return (long) (int) ((x & -x) - 1u);
}

int
main (void)
{
  if (f(0x100000000) != -1)
    __builtin_abort ();
  __builtin_exit (0);
}
