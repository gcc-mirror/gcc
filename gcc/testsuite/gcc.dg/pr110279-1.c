/* { dg-do compile { target { scalar_all_fma || { i?86-*-* x86_64-*-* } } } } */
/* { dg-options "-Ofast --param avoid-fma-max-bits=512 --param tree-reassoc-width=4 -fdump-tree-widening_mul-details" } */
/* { dg-additional-options "-mcpu=generic" { target aarch64*-*-* } } */
/* { dg-additional-options "-mfma" { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-march=2.0" { target hppa*-*-* } } */

#define LOOP_COUNT 800000000
typedef double data_e;

/* Check that FMAs with backedge dependency are avoided. Otherwise there won't
   be FMA generated with "--param avoid-fma-max-bits=512".   */

data_e
foo1 (data_e a, data_e b, data_e c, data_e d)
{
  data_e result = 0;

  for (int ic = 0; ic < LOOP_COUNT; ic++)
    {
      result += (a * b + c * d);

      a -= 0.1;
      b += 0.9;
      c *= 1.02;
      d *= 0.61;
    }

  return result;
}

data_e
foo2 (data_e a, data_e b, data_e c, data_e d)
{
  data_e result = 0;

  for (int ic = 0; ic < LOOP_COUNT; ic++)
    {
      result = a * b + result + c * d;

      a -= 0.1;
      b += 0.9;
      c *= 1.02;
      d *= 0.61;
    }

  return result;
}

data_e
foo3 (data_e a, data_e b, data_e c, data_e d)
{
  data_e result = 0;

  for (int ic = 0; ic < LOOP_COUNT; ic++)
    {
      result = result + a * b + c * d;

      a -= 0.1;
      b += 0.9;
      c *= 1.02;
      d *= 0.61;
    }

  return result;
}

/* { dg-final { scan-tree-dump-times "Generated FMA" 3 "widening_mul" } } */
