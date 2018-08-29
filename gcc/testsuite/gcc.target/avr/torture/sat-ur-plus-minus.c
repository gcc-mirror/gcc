/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-options "-std=gnu99 -fwrapv" } */

#include "fix-types.h"

extern void abort (void);
extern void exit (int);

typedef unsigned _Fract fx_t;
typedef unsigned _Sat _Fract satfx_t;
typedef unsigned int intfx_t;

US_LFUN (us_add, +, fx_t, ur, >)
US_LFUN (us_sub, -, fx_t, ur, <)

#define VAL(N, X)                               \
    __attribute__((noinline,noclone))           \
    satfx_t us_add2_##N (satfx_t a)             \
    {                                           \
        return us_add_ur (a, X##P##-##16ur);    \
    }                                           \
    __attribute__((noinline,noclone))           \
    satfx_t us_add_##N (satfx_t a)              \
    {                                           \
        return a + X##P##-##16ur;               \
    }                                           \
    __attribute__((noinline,noclone))           \
    satfx_t us_sub2_##N (satfx_t a)             \
    {                                           \
        return us_sub_ur (a, X##P##-##16ur);    \
    }                                           \
    __attribute__((noinline,noclone))           \
    satfx_t us_sub_##N (satfx_t a)              \
    {                                           \
        return a - X##P##-##16ur;               \
    }
#include "vals-ur.def"
#undef VAL

satfx_t (* __flash const fun[])(satfx_t) =
{
#define VAL(N, X)                               \
  us_add_##N, us_add2_##N,                      \
  us_sub_##N, us_sub2_##N,
#include "vals-ur.def"
#undef VAL
};


const volatile __flash intfx_t vals[] =
  {
    0, -1, 1, -2, 2, -127, -128, -129,
    0x7f, 0x80, 0x81, 0x100,
    0x4000, 0x3e80, 0x3f80,
    0x7ffe, 0x7fff,
    0x7f7f, 0x7f81, 0x7f80,
    0x7f01,
    0x8000, 0x8001, 0x8080,
    0x8081, 0x80ff, 0x80fe,
    0x8100, 0x8180, 0x817f,
    0xff00, 0xff01, 0xff01,
    0xff7f, 0xff80
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
