/* { dg-options "-fPIC" { target { ! sparc*-*-* } } } */
static void
f (int n)
{
  int i;
  static int __thread value = 100;
  for (i = 0; i < n; ++i)
    {
      volatile int *p = &value;
      volatile int x __attribute__ ((unused)) = *p;
    }
}


extern int foo (int);

int
main (int argc, char **argv)
{
  f (foo (4) + argc);
  return 0;
}
