/* PR 5967, PR 7114 */
/* { dg-do run } */
/* { dg-options "-O2 -pg" } */
/* { dg-error "profiler" "No profiler support" { target mmix-*-* } 0 } */
/* Support for -pg on irix relies on gcrt1.o which doesn't exist yet.
   See: http://gcc.gnu.org/ml/gcc/2002-10/msg00169.html */
/* { dg-error "gcrt1.o" "Profiler support missing" { target mips*-*-irix* } 0 } */
/* { dg-error "-pg not supported" "Profiler support missing" { target *-*-sco3.2v5* } 0 } */

long foo (long x)
{
  long i, sum = 0;
  long bar (long z) { return z * 2; }

  for (i = 0; i < x; i++)
    sum += bar (i);

  return sum;
}

int main (void)
{
  if (foo(10) != 90)
    abort ();
  return 0;
}
