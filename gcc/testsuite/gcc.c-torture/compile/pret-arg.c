/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */
/* { dg-additional-options "-std=gnu89" } */

foo (a, b, c, d, e, f, g, h, i, j, xx)
     double xx;
{
  return xx + 1.2345;
}
