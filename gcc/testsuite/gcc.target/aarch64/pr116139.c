/* PR tree-optimization/116139 */
/* { dg-do compile } */
/* { dg-options "-Ofast --param fully-pipelined-fma=1 -mcpu=neoverse-n3" } */

#define LOOP_COUNT 800000000
typedef double data_e;

data_e
foo (data_e in)
{
  data_e a1, a2, a3, a4;
  data_e tmp, result = 0;
  a1 = in + 0.1;
  a2 = in * 0.1;
  a3 = in + 0.01;
  a4 = in * 0.59;

  data_e result2 = 0;

  for (int ic = 0; ic < LOOP_COUNT; ic++)
    {
      tmp = a1 + a2 * a2 + a3 * a3 + a4 * a4 ;
      result += tmp - ic;
      result2 = result2 / 2 - tmp;

      a1 += 0.91;
      a2 += 0.1;
      a3 -= 0.01;
      a4 -= 0.89;

    }

  return result + result2;
}

