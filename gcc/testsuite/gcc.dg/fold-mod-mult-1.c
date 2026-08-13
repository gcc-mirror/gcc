/* (t * u) % u is zero whenever the product does not overflow: for signed
   types the overflow is undefined so it always folds; for unsigned types it
   folds when value ranges prove the multiply cannot wrap.  */
/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
f_signed (int a, int b)
{
  return (a * b) % b;
}

unsigned
f_ranged (unsigned a, unsigned b)
{
  a &= 0xffff;
  b &= 0xffff;
  if (b == 0)
    return 7;
  return (a * b) % b;		/* a*b <= 0xfffe0001, cannot wrap */
}

/* Both remainders fold away; no modulo survives.  */
/* { dg-final { scan-tree-dump-not " % " "optimized" } } */
