/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple" } */

int
foo (int align)
{
  int off = 0 % align;
  return off ? align - off : 0;
}

/* We should have optimized away the mod operator before we gimpleized
   the code.  */
/* { dg-final { scan-tree-dump-times "%" 0 "gimple"} } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
