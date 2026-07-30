/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned int u32;

/* Keep the MIN live so that replacing the addition would add work.  */

u32
add_min_live (u32 a, u32 b, u32 *out)
{
  u32 t = ~a;
  u32 m = b < t ? b : t;
  *out = m;
  return a + m;
}

/* { dg-final { scan-tree-dump-not "\\.SAT_ADD " "optimized" } } */
