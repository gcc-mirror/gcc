/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-generic" } */

int
foo (int align)
{
  int off = 0 % align;
  return off ? align - off : 0;
}

/* We should have optimized away the mod operator before we genericized
   the code.  */
/* { dg-final { scan-tree-dump-times "%" 0 "generic"} } */
