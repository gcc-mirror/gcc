/* Two single-bit tests of the same value combine into one masked compare.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
t_and_set_set (unsigned x)		/* (x&8)!=0 & (x&16)!=0 -> (x&24)==24 */
{
  return ((x & 8) != 0) & ((x & 16) != 0);
}

int
t_and_set_clr (unsigned x)		/* (x&8)!=0 & (x&16)==0 -> (x&24)==8 */
{
  return ((x & 8) != 0) & ((x & 16) == 0);
}

int
t_or_clr_clr (unsigned x)		/* (x&8)==0 | (x&16)==0 -> (x&24)!=24 */
{
  return ((x & 8) == 0) | ((x & 16) == 0);
}

int
t_or_set_clr (unsigned x)		/* (x&8)!=0 | (x&16)==0 -> (x&24)!=16 */
{
  return ((x & 8) != 0) | ((x & 16) == 0);
}

/* Each becomes a single (x & 24) compare; the separate & 8 / & 16 masks and
   the boolean combiner are gone.  */
/* { dg-final { scan-tree-dump-times " & 24;" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-not " & 8;" "optimized" } } */
/* { dg-final { scan-tree-dump-not " & 16;" "optimized" } } */
