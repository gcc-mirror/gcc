/* { dg-do run { target int128 } } */

static unsigned __attribute__((noinline, noclone))
foo (unsigned long long u)
{
  unsigned __int128 v = u | 0xffffff81U;
  v >>= 64;
  return v;
}

int
main ()
{
  unsigned x = foo (27);
  if (x != 0)
    __builtin_abort ();
  return 0;
}
