/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

_Bool f1(_Bool x)
{
  return !!x;
}

/* There should be no != 0 which is produced by the front-end as
   bool_var != 0 is the same as bool_var. */
/* { dg-final { scan-tree-dump-times "!= 0" 0 "optimized"} } */

/* There should be no subfe for powerpc. Checking if we actually optimizated
   away the comparision.  */
/* { dg-final { scan-assembler-times "subfe" 0 { target powerpc-*-* } } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
