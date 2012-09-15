/* { dg-do run } */
/* { dg-options "-std=gnu99 -fwrapv" } */

#include "fix-types.h"

extern void abort (void);
extern void exit (int);

typedef _Accum fx_t;
typedef _Sat _Accum satfx_t;
typedef long intfx_t;

SS_FUN (ss_add, +, fx_t, k)
SS_FUN (ss_sub, -, fx_t, k)

#define VAL(N, X)                               \
  __attribute__((noinline,noclone))             \
  satfx_t ss_add2_##N (satfx_t a)               \
  {                                             \
    return ss_add_k (a, X##P##-##16k);          \
  }                                             \
  __attribute__((noinline,noclone))             \
  satfx_t ss_add_##N (satfx_t a)                \
  {                                             \
    return a + X##P##-##16k;                    \
  }                                             \
  __attribute__((noinline,noclone))             \
  satfx_t ss_sub2_##N (satfx_t a)               \
  {                                             \
    return ss_sub_k (a, X##P##-##16k);          \
  }                                             \
  __attribute__((noinline,noclone))             \
  satfx_t ss_sub_##N (satfx_t a)                \
  {                                             \
    return a - X##P##-##16k;                    \
  }
#include "vals-k.def"
#undef VAL

__attribute__((noinline,noclone))
satfx_t ss_add2_99 (satfx_t a)
{
  return ss_add_k (a, __ACCUM_MIN__);
}

__attribute__((noinline,noclone))
satfx_t ss_add_99 (satfx_t a)
{
  return a + __ACCUM_MIN__;
}

__attribute__((noinline,noclone))
satfx_t ss_sub2_99 (satfx_t a)
{
  return ss_sub_k (a, __ACCUM_MIN__);
}

__attribute__((noinline,noclone))
satfx_t ss_sub_99 (satfx_t a)
{
  return a - __ACCUM_MIN__;
}


satfx_t (* __flash const fun[])(satfx_t) =
{
#define VAL(N, X)                               \
  ss_add_##N, ss_add2_##N,                      \
  ss_sub_##N, ss_sub2_##N,
#include "vals-k.def"
  VAL (99,)
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
