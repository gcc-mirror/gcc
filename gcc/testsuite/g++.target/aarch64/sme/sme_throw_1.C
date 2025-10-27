/* { dg-do run { target { aarch64*-linux-gnu* && aarch64_sme_hw } } } */

#include <signal.h>
#include <arm_sme.h>

static bool caught;

[[gnu::noipa]] void thrower(int)
{
  throw 1;
}

[[gnu::noipa]] void bar()
{
  *(volatile int *)0 = 0;
}

[[gnu::noipa]] void foo()
{
  try
    {
      bar();
    }
  catch (int)
    {
      caught = true;
    }
}

__arm_new("za") __arm_locally_streaming void sme_user()
{
  svbool_t all = svptrue_b8();
  for (unsigned int i = 0; i < svcntb(); ++i)
    {
      svint8_t expected = svindex_s8(i + 1, i);
      svwrite_hor_za8_m(0, i, all, expected);
    }
  foo();
  for (unsigned int i = 0; i < svcntb(); ++i)
    {
      svint8_t expected = svindex_s8(i + 1, i);
      svint8_t actual = svread_hor_za8_m(svdup_s8(0), all, 0, i);
      if (svptest_any(all, svcmpne(all, expected, actual)))
	__builtin_abort();
    }
  if (!caught)
    __builtin_abort();
}

int main()
{
  signal(SIGSEGV, thrower);
  sme_user();
  return 0;
}
