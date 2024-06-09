/* PR tree-optimization/110279 */
/* { dg-do compile } */
/* { dg-options "-Ofast --param tree-reassoc-width=4 --param fully-pipelined-fma=1 -fdump-tree-reassoc2-details -fdump-tree-optimized" } */
/* { dg-additional-options "-mcpu=generic" { target aarch64*-*-* } } */

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
      /* Test that a complete FMA chain with length=4 is not broken.  */
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

/* { dg-final { scan-tree-dump-not "was chosen for reassociation" "reassoc2" { target aarch64*-*-* }} } */
/* { dg-final { scan-tree-dump-times {\.FMA } 3 "optimized" { target aarch64*-*-* }} } */