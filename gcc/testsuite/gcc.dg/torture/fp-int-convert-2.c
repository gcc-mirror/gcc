/* { dg-do run } */
/* { dg-require-effective-target int128 } */

extern void abort (void);

float __attribute__((noinline))
f (__uint128_t x)
{
  return x + 1;
}

int
main (void)
{
  if (f (0xffffffffu) == 0)
    abort ();
  return 0;
}
