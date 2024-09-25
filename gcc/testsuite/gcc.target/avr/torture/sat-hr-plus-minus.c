/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-options "-std=gnu99 -fwrapv" } */

#include "fix-types.h"

extern void abort (void);
extern void exit (int);

typedef short _Fract fx_t;
typedef short _Sat _Fract satfx_t;
typedef char intfx_t;

SS_FUN (ss_add, +, fx_t, hr)
SS_FUN (ss_sub, -, fx_t, hr)

#define VAL(N, X)                               \
    __attribute__((noipa))                      \
    satfx_t ss_add2_##N (satfx_t a)             \
    {                                           \
        return ss_add_hr (a, X##P##-##7hr);     \
    }                                           \
    __attribute__((noipa))                      \
    satfx_t ss_add_##N (satfx_t a)              \
    {                                           \
        return a + X##P##-##7hr;                \
    }                                           \
    __attribute__((noipa))                      \
    satfx_t ss_sub2_##N (satfx_t a)             \
    {                                           \
        return ss_sub_hr (a, X##P##-##7hr);     \
    }                                           \
    __attribute__((noipa))                      \
    satfx_t ss_sub_##N (satfx_t a)              \
    {                                           \
        return a - X##P##-##7hr;                \
    }
#include "vals-hr.def"
#undef VAL

__attribute__((noipa))
satfx_t ss_add2_99 (satfx_t a)
{
  return ss_add_hr (a, __FRACT_MIN__);
}

__attribute__((noipa))
satfx_t ss_add_99 (satfx_t a)
{
  return a + __FRACT_MIN__;
}

__attribute__((noipa))
satfx_t ss_sub2_99 (satfx_t a)
{
  return ss_sub_hr (a, __FRACT_MIN__);
}

__attribute__((noipa))
satfx_t ss_sub_99 (satfx_t a)
{
  return a - __FRACT_MIN__;
}


satfx_t (* __flash const fun[])(satfx_t) =
{
#define VAL(N, X)                               \
  ss_add_##N, ss_add2_##N,                      \
  ss_sub_##N, ss_sub2_##N,
#include "vals-hr.def"
  VAL (99,)
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
