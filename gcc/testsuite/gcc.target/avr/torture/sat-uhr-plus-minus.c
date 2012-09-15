/* { dg-do run } */
/* { dg-options "-std=gnu99 -fwrapv" } */

#include "fix-types.h"

extern void abort (void);
extern void exit (int);

typedef unsigned short _Fract fx_t;
typedef unsigned short _Sat _Fract satfx_t;
typedef unsigned char intfx_t;

US_LFUN (us_add, +, fx_t, uhr, >)
US_LFUN (us_sub, -, fx_t, uhr, <)

#define VAL(N, X)                               \
  __attribute__((noinline,noclone))             \
  satfx_t us_add2_##N (satfx_t a)               \
  {                                             \
    return us_add_uhr (a, X##P##-##8uhr);       \
  }                                             \
  __attribute__((noinline,noclone))             \
  satfx_t us_add_##N (satfx_t a)                \
  {                                             \
    return a + X##P##-##8uhr;                   \
  }                                             \
  __attribute__((noinline,noclone))             \
  satfx_t us_sub2_##N (satfx_t a)               \
  {                                             \
    return us_sub_uhr (a, X##P##-##8uhr);       \
  }                                             \
  __attribute__((noinline,noclone))             \
  satfx_t us_sub_##N (satfx_t a)                \
  {                                             \
    return a - X##P##-##8uhr;                   \
  }
#include "vals-uhr.def"
#undef VAL

satfx_t (* __flash const fun[])(satfx_t) =
{
#define VAL(N, X)                               \
  us_add_##N, us_add2_##N,                      \
  us_sub_##N, us_sub2_##N,
#include "vals-uhr.def"
#undef VAL
};


const volatile __flash intfx_t vals[] =
  {
    0, 1, 2, 0x7f, 0x80, 0x81, 0xff,
    0x40, 0x3e, 0x3f, 0xbf, 0xc0, 0xc1
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
