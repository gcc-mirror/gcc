/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -fno-vect-cost-model" } */

int a[35] = { 1, 1, 3 };

void
foo ()
{
  for (int b = 4; b >= 0; b--)
    {
      int tem = a[b * 5 + 3 + 1];
      a[b * 5 + 3] = tem;
      a[b * 5 + 2] = tem;
      a[b * 5 + 1] = tem;
      a[b * 5 + 0] = tem;
    }
}

/* Since the regmode-natural-size changes we cannot build half-vector subregs
   of regs.  Xfail this until we have a better way of describing and especially
   querying vec_extract support.  */

/* { dg-final { scan-assembler-times "vslidedown" 2 { xfail *-*-* } } } */
