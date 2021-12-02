/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
unsigned char f(unsigned char a)
{
  signed char d = (signed char) a;
  signed char e = d & ~1;
  unsigned char t = e;
  t &= ~2;
  return t;
}
/* The above should reduce down to just & 252 rather than keping
   the two &s there. */
/* { dg-final { scan-tree-dump-times "& 252" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "& -2" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "& 253" 0 "optimized"} } */
