/* { dg-options "-fdelayed-branch" { target sparc*-*-* } } */

extern void abort (void);

int __attribute__((noinline,noclone))
callee (int i)
{
  return i * i;
}

int __attribute__((noinline,noclone))
caller (int i)
{
  return callee (i + 1);
}

int
main (int argc, const char **argv)
{
  int result = caller (5);
  if (result != 36)
    abort ();
  return 0;
}
