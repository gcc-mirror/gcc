/* { dg-lto-do run } */
/* { dg-lto-options { { -O2 -flto=auto } } } */
/* { dg-extra-ld-options { -flto-partition=1to1 } } */

extern __attribute__((noinline))
void foo (int *p);


void __attribute__((noinline))
bar (void)
{
  int istat;

  istat = 1234;
  foo (&istat);
  if (istat != 1234)
    __builtin_abort ();
}

int main (int argc, char **argv)
{
  bar ();
  return 0;
}
