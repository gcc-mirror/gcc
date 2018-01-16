/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-options "-std=gnu99 -fwrapv" } */

#include "fix-types.h"

extern void abort (void);
extern void exit (int);

typedef unsigned _Accum fx_t;
typedef unsigned _Sat _Accum satfx_t;
typedef unsigned long intfx_t;

US_LFUN (us_add, +, fx_t, uk, >)
US_LFUN (us_sub, -, fx_t, uk, <)

#define VAL(N, X)                               \
    __attribute__((noinline,noclone))           \
    satfx_t us_add2_##N (satfx_t a)             \
    {                                           \
        return us_add_uk (a, X##P##-##16uk);    \
    }                                           \
    __attribute__((noinline,noclone))           \
    satfx_t us_add_##N (satfx_t a)              \
    {                                           \
        return a + X##P##-##16uk;               \
    }                                           \
    __attribute__((noinline,noclone))           \
    satfx_t us_sub2_##N (satfx_t a)             \
    {                                           \
        return us_sub_uk (a, X##P##-##16uk);    \
    }                                           \
    __attribute__((noinline,noclone))           \
    satfx_t us_sub_##N (satfx_t a)              \
    {                                           \
        return a - X##P##-##16uk;               \
    }
#include "vals-uk.def"
#undef VAL

satfx_t (* __flash const fun[])(satfx_t) =
{
#define VAL(N, X)                               \
  us_add_##N, us_add2_##N,                      \
  us_sub_##N, us_sub2_##N,
#include "vals-uk.def"
#undef VAL
};


const volatile __flash intfx_t vals[] =
  {
    0, -1, 1, -2, 2, -127, -128, -129,
    0x7f, 0x80, 0x81, 0x100,
    0x40000000, 0x3e800000, 0x3f800000,
    0x7ffffffe, 0x7fffffff, 0x7f800000,
    0x7f7f7f7f, 0x7f810080, 0x7f008000,
    0x7f000001,
    0x80000000, 0x80000001, 0x80808080,
    0x80810000, 0x80ffffff, 0x80fffffe,
    0x81000000, 0x81800000, 0x81800000,
    0xff000000, 0xffffff01, 0xffffff80,
    0xffffff7f, 0xff80ff80
  };


int main (void)
{
  for (unsigned int i = 0; i < sizeof (vals) / sizeof (*vals); i++)
    {
      satfx_t a, f1, f2;
      intfx_t val = vals[i];
      __builtin_memcpy (&a, &val, sizeof (satfx_t));
      for (unsigned int f = 0; f < sizeof (fun) / sizeof (*fun); f += 2)
        {
          if (fun[f](a) != fun[f+1](a))
            abort();
        }
    }

  exit (0);
  return 0;
}
