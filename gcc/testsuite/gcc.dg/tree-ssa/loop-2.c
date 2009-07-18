/* A test for strength reduction and induction variable elimination.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
/* { dg-require-effective-target size32plus } */

/* Size of this structure should be sufficiently weird so that no memory
   addressing mode applies.  */

struct bla
{
  char x[187];
  int y;
  char z[253];
} arr_base[100];

void xxx(void)
{
  int iter;

  for (iter = 0; iter < 100; iter++)
    arr_base[iter].y = 17 * iter;
}

/* Access to arr_base[iter].y should be strength reduced, i.e., there should
   be no multiplication.  */

/* { dg-final { scan-tree-dump-times " \\* \[^\\n\\r\]*=" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\[^\\n\\r\]*= \\* " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MEM" 1 "optimized" } } */

/* 17 * iter should be strength reduced.  */

/* { dg-final { scan-tree-dump-times " \\* 17" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\+ 17" 1 "optimized" } } */

/* The induction variable comparison with 99 should be eliminated
   and replaced by comparison of one of the newly created ivs.  */

/* { dg-final { scan-tree-dump-times "iter" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= 99" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "< 100" 0 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
