/* { dg-do compile } */
/* { dg-require-effective-target popcount } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned neg(unsigned uword, unsigned other)
{
  uword = (uword & 0x55555555u) + ((uword >> 1) & 0x55555555u);
  uword = (uword & 0x33333333u) + ((uword >> 2) & 0x33333333u);
  uword = (uword & 0x0f0f0f0fu) + ((uword >> 4) & 0x0f0f0f0fu);
  uword = (uword & 0x00ff00ffu) + ((uword >> 8) & 0x00ff00ffu);
  return (other & 0x0000ffffu) + (uword >> 16);
}

/* { dg-final { scan-tree-dump-not "\.POPCOUNT" "optimized" } } */
