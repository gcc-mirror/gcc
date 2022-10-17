/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-expensive-optimizations" } */

__int128 m;
int n;

__attribute__ ((noinline)) int
return_zero (void)
{
  return 0;
}

void
foo (void)
{
  while (m < 0)
    {
      if (n || return_zero ())
        __builtin_trap ();

      ++m;
    }
}

