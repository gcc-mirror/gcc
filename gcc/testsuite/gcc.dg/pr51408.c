/* This testcase used to fail because of a bug in 
   arm.md:*minmax_arithsi.  */

/* { dg-do run } */
/* { dg-options "-O1" } */

extern void abort (void);

int __attribute__((noinline))
foo (int a, int b)
{
  int max = (b > 0) ? b : 0;
  return max - a;
}

int
main (void)
{
  if (foo (3, -1) != -3)
    abort ();
  return 0;
}
