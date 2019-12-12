/* PR tree-optimization/91734 */
/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-additional-options "-O2 -std=gnu99" } */

__attribute__((noipa, optimize ("Ofast"))) int
f1 (float x)
{
  return __builtin_sqrtf (x) < __FLT_MIN__;
}

__attribute__((noipa, optimize ("Ofast"))) int
f2 (float x)
{
  return __builtin_sqrtf (x) < 0x1.2dd3d0p-65f;
}

__attribute__((noipa, optimize ("Ofast"))) int
f3 (float x)
{
  return __builtin_sqrtf (x) >= 0x1.2dd3d0p-65f;
}

__attribute__((noipa, optimize ("Ofast"))) int
f4 (float x)
{
  return __builtin_sqrtf (x) >= 0x1.5642e6p+54f;
}

__attribute__((noipa, optimize ("Ofast"))) int
f5 (float x)
{
  return __builtin_sqrtf (x) > 0x1.5642e6p+54f;
}

__attribute__((noipa, optimize ("Ofast"))) int
f6 (float x)
{
  return __builtin_sqrtf (x) < 0x1.4da1cp-19f;
}

__attribute__((noipa, optimize ("Ofast"))) int
f7 (float x)
{
  return __builtin_sqrtf (x) <= 0x1.4da1cp-19f;
}

__attribute__((noipa, optimize ("Ofast"))) int
f8 (float x)
{
  return __builtin_sqrtf (x) < 0x1.50cb62p-65f;
}

__attribute__((noipa, optimize ("Ofast"))) int
f9 (float x)
{
  return __builtin_sqrtf (x) <= 0x1.4fc00cp-73f;
}

__attribute__((noipa, optimize ("Ofast"))) int
f10 (float x)
{
  return __builtin_sqrtf (x) < 0x1.001002p+0f;
}

int
main ()
{
  if (__FLT_RADIX__ != 2
      || __FLT_MANT_DIG__ != 24
      || __FLT_MIN_EXP__ != -125
      || __FLT_MAX_EXP__ != 128
      || __FLT_HAS_DENORM__ != 1
      || __FLT_HAS_INFINITY__ != 1)
    return 0;
  if (!f1 (0.0f) || f1 (0x1.0p-149f))
    __builtin_abort ();
  if (!f2 (0x1.63dbc0p-130f))
    __builtin_abort ();
  if (f3 (0x1.63dbc0p-130f))
    __builtin_abort ();
  if (!f4 (0x1.c996d0p+108f) || !f4 (0x1.c996cep+108f) || f4 (0x1.c996ccp+108f))
    __builtin_abort ();
  if (f5 (0x1.c996d0p+108f) || f5 (0x1.c996d2p+108f) || !f5 (0x1.c996d4p+108f))
    __builtin_abort ();
  if (!f6 (0x1.b2ce3p-38f) || f6 (0x1.b2ce32p-38f) || f6 (0x1.b2ce34p-38f))
    __builtin_abort ();
  if (!f7 (0x1.b2ce3p-38f) || !f7 (0x1.b2ce34p-38f) || !f7 (0x1.b2ce36p-38f) || f7 (0x1.b2ce38p-38f))
    __builtin_abort ();
  if (!f8 (0x1.bb166p-130f) || !f8 (0x1.bb168p-130f) || f8 (0x1.bb16ap-130f) || f8 (0x1.bb16cp-130f))
    __builtin_abort ();
  if (!f9 (0x1.8p-146f) || !f9 (0x1.ap-146f) || f9 (0x1.cp-146f) || f9 (0x1.ep-146f))
    __builtin_abort ();
  if (f10 (0x1.002004p+0f))
    __builtin_abort ();
  return 0;
}
