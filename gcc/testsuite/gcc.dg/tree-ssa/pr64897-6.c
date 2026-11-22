/* PR target/64897 */
/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Decimal floats are left alone.  */

_Decimal64
abs_dd (_Decimal64 x)
{
  __UINT64_TYPE__ i;
  __builtin_memcpy (&i, &x, sizeof i);
  i &= 0x7fffffffffffffffull;
  __builtin_memcpy (&x, &i, sizeof x);
  return x;
}

/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized" } } */
