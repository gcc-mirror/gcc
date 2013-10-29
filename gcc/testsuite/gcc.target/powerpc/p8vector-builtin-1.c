/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O2 -ftree-vectorize -fvect-cost-model=dynamic -fno-unroll-loops -fno-unroll-all-loops" } */

#ifndef TYPE
#define TYPE long long
#endif

#ifndef SIGN_TYPE
#define SIGN_TYPE signed TYPE
#endif

#ifndef UNS_TYPE
#define UNS_TYPE unsigned TYPE
#endif

typedef vector SIGN_TYPE v_sign;
typedef vector UNS_TYPE  v_uns;

v_sign sign_add (v_sign a, v_sign b)
{
  return a + b;
}

v_sign sign_sub (v_sign a, v_sign b)
{
  return a - b;
}

v_sign sign_shift_left (v_sign a, v_sign b)
{
  return a << b;
}

v_sign sign_shift_right (v_sign a, v_sign b)
{
  return a >> b;
}

v_uns uns_add (v_uns a, v_uns b)
{
  return a + b;
}

v_uns uns_sub (v_uns a, v_uns b)
{
  return a - b;
}

v_uns uns_shift_left (v_uns a, v_uns b)
{
  return a << b;
}

v_uns uns_shift_right (v_uns a, v_uns b)
{
  return a >> b;
}

/* { dg-final { scan-assembler-times "vaddudm" 2 } } */
/* { dg-final { scan-assembler-times "vsubudm" 2 } } */
/* { dg-final { scan-assembler-times "vsld"    2 } } */
/* { dg-final { scan-assembler-times "vsrad"   1 } } */
/* { dg-final { scan-assembler-times "vsrd"    1 } } */
