/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f(_Bool x)
{
  return (x != 1);
}

/* There should be no != 1 which is produced by the front-end as
   bool_var != 1 is the same as !bool_var. */
/* { dg-final { scan-tree-dump-times "!= 1" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "~x" 1 "optimized"} } */

