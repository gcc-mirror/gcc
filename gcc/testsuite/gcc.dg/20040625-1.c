/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-mminimal-toc" { target powerpc64-*-* } } */

extern void abort (void);
extern void exit (int);

double __attribute__((noinline))
foo (void)
{
  return 16441577472.0;
}

double __attribute__((noinline))
bar (double x)
{
  return x;
}

int __attribute__((noinline))
test (void)
{
  double x = foo ();
  x = bar (x);
  x /= 1024 * 1024 * 1024;
  x *= 70;
  x = x < 70 ? x : 70;
  x += 30;
  return x;
}

int main (void)
{
  if (test () != 100)
    abort ();
  exit (0);
}
