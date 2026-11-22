/* PR target/64897 */
/* { dg-do compile } */
/* { dg-require-effective-target float16 } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options float16 } */

/* The same for _Float16, whose sign bit is bit 15.  */

_Float16
abs_h (_Float16 x)
{
  __UINT16_TYPE__ i;
  __builtin_memcpy (&i, &x, sizeof i);
  i &= 0x7fff;
  __builtin_memcpy (&x, &i, sizeof x);
  return x;
}

_Float16
neg_h (_Float16 x)
{
  __UINT16_TYPE__ i;
  __builtin_memcpy (&i, &x, sizeof i);
  i ^= 0x8000;
  __builtin_memcpy (&x, &i, sizeof x);
  return x;
}

/* { dg-final { scan-tree-dump-times "ABS_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " = -x_" 1 "optimized" } } */
