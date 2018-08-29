/* { dg-do run } */
/* { dg-options "-O2 -march=i686 -mtune=generic -fstack-clash-protection" } */
/* { dg-require-effective-target ia32 } */

__attribute__ ((noinline, noclone, weak, regparm (3)))
int
f1 (long arg0, int (*pf) (long, void *))
{
  unsigned char buf[32768];
  return pf (arg0, buf);
}

__attribute__ ((noinline, noclone, weak))
int
f2 (long arg0, void *ignored)
{
  if (arg0 != 17)
    __builtin_abort ();
  return 19;
}

int
main (void)
{
  if (f1 (17, f2) != 19)
    __builtin_abort ();
  return 0;
}


