/* PR middle-end/114753 */
/* { dg-do run } */
/* { dg-options "-O2 -ftrapv" } */

int
main ()
{
  volatile long long i = __LONG_LONG_MAX__;
  volatile long long j = 2;
  long long k;
  if (!__builtin_mul_overflow (i, j, &k) || k != -2LL)
    __builtin_abort ();
  return 0;
}
