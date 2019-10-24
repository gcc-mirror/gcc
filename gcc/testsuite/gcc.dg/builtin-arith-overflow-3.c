/* { dg-do run } */
/* { dg-options "-O2" } */

static int cnt = 0;

#define LL_MIN ((long long)(-__LONG_LONG_MAX__ - 1))

#define SC1 (LL_MIN + 5)
#define UC1 ((1ULL << (__LONG_LONG_WIDTH__ - 1)) | 5ULL)
#define UC2 (~UC1)

long long __attribute__ ((noinline, noclone))
f1 (long long a)
{
  long long x;
  if (__builtin_add_overflow (a, SC1, &x)) cnt++;
  return x;
}

unsigned long long __attribute__ ((noinline, noclone))
f2 (unsigned long long a)
{
  unsigned long long x;
  if (__builtin_add_overflow (a, UC1, &x))
    cnt++;
  return x;
}

int main ()
{
  if (f1 (-5) != LL_MIN) __builtin_abort ();
  if (cnt != 0) __builtin_abort ();
  f1 (-6);
  if (cnt != 1) __builtin_abort ();
  cnt = 0;
  if (f2 (UC2) != ~0ULL) __builtin_abort ();
  if (cnt != 0) __builtin_abort ();
  if (f2 (UC2 + 1) != 0) __builtin_abort ();
  if (cnt != 1) __builtin_abort ();
  return 0;
}
