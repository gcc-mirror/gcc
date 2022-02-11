/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
signed char f(unsigned char a)
{
  unsigned char b = a & 127;
  signed char c = (signed char) b;
  signed char d = (signed char) a;
  signed char e = d & -128;
  signed char h = c | e;
  return h;
}
/* The above should reduce down to just return with a cast.
   removing the two &s there and |'s. */
/* { dg-final { scan-tree-dump-times "& 127" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "& -128" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "\\\| " 0 "optimized"} } */
