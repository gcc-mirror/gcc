/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-skip-if "non-optimized code is too slow" { ! run_expensive_tests } { "*" } { "-O2" "-O3" } } */

int
main ()
{
  long long b = 2036854775807LL;
  signed char c = 3;
  short d = 0;
  int e = -2147483647 - 1, f;
  for (f = 0; f < 7; f++)
    while (e < 20)
      {
	e += 2;
	d = c -= b;
      }
  if (d != 13)
    __builtin_abort ();
  return 0;
}
