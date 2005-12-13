/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f(_Bool x)
{
  if (x != 0)
   return 1;
  return 0;
}

/* There should be no != 0 which is produced by the front-end as
   bool_var != 0 is the same as bool_var. */
/* { dg-final { scan-tree-dump-times "!= 0" 0 "optimized"} } */

/* There should be no adde for powerpc. Checking if we actually optimizated
   away the comparision.  */
/* { dg-final { scan-assembler-times "adde" 0 { target powerpc*-*-* } } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
