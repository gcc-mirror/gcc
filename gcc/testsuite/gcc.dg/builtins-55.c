/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

#include "builtins-config.h"

void link_error (void);

extern long lround(double);
extern long lrint(double);

extern long long llround(double);
extern long long llrint(double);

extern long lroundf(float);
extern long lrintf(float);

extern long long llroundf(float);
extern long long llrintf(float);

extern long lroundl(long double);
extern long lrintl(long double);

extern long long llroundl(long double);
extern long long llrintl(long double);


void test(double x)
{
#ifdef HAVE_C99_RUNTIME
  if (sizeof(long) != sizeof(long long))
    return;

  if (__builtin_lceil(x) != __builtin_llceil(x))
    link_error();
  if (__builtin_lfloor(x) != __builtin_llfloor(x))
    link_error();
  if (lround(x) != llround(x))
    link_error();
  if (lrint(x) != llrint(x))
    link_error();
#endif
}

void testf(float x)
{
#ifdef HAVE_C99_RUNTIME
  if (sizeof(long) != sizeof(long long))
    return;

  if (__builtin_lceilf(x) != __builtin_llceilf(x))
    link_error();
  if (__builtin_lfloorf(x) != __builtin_llfloorf(x))
    link_error();
  if (lroundf(x) != llroundf(x))
    link_error();
  if (lrintf(x) != llrintf(x))
    link_error();
#endif
}

void testl(long double x)
{
#ifdef HAVE_C99_RUNTIME
  if (sizeof(long) != sizeof(long long))
    return;

  if (__builtin_lceill(x) != __builtin_llceill(x))
    link_error();
  if (__builtin_lfloorl(x) != __builtin_llfloorl(x))
    link_error();
  if (lroundl(x) != llroundl(x))
    link_error();
  if (lrintl(x) != llrintl(x))
    link_error();
#endif
}

int main()
{
  test(0.0);
  testf(0.0);
  testl(0.0);
  return 0;
}

