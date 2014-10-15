/* { dg-do run { target fpic } } */
/* { dg-options "-O2 -fpic" } */

extern void abort (void);
extern void exit (int);

double
foo (void)
{
  return (__extension__ ((union { unsigned __l __attribute__((__mode__(__SI__))); float __d; }) { __l: 0x3f800000UL }).__d);
}

int
main ()
{
  if (foo() != 1.0)
    abort ();
  exit (0);
}
