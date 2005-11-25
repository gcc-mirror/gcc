/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f(_Bool x)
{
  return (x == 0);
}

/* There should be no == 0 which is produced by the front-end as
   bool_var == 0 is the same as !bool_var. */
/* { dg-final { scan-tree-dump-times "== 0" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "!x" 1 "optimized"} } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
