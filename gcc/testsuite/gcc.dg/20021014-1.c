/* { dg-do run } */
/* { dg-options "-O2 -p" } */
/* { dg-build "profiler" "No profiler support" { xfail mmix-*-* } } */

extern void abort (void);
extern void exit (int);

int foo (void)
{
  static int bar (int x)
  {
    return x + 3;
  }
  return bar (1) + bar (2);
}

int main (void)
{
  if (foo () != 9)
    abort ();
  exit (0);
}
