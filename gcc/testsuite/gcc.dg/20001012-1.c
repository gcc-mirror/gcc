/* { dg-do run { target sparc*-*-* } } */
/* { dg-options "-O2 -fpic" } */

double
foo (void)
{
  return (__extension__ ((union { unsigned __l __attribute__((__mode__(__SI__))); float __d; }) { __l: 0x3f800000UL }).__d);
}

main ()
{
  if (foo() != 1.0)
    abort ();
  exit (0);
}
