/* PR middle-end/95903 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fwrapv -fdump-tree-optimized" } */
/* Verify that for -fwrapv the + 1 addition is performed in the parameter's
   type before sign extending it.  */
/* { dg-final { scan-tree-dump-times "off_\[0-9]+\\\(D\\\) \\+ 1" 2 "optimized" } } */

char
foo (const char *ptr, int off)
{
  off += 1;
  return ptr[off];
}

char
bar (const char *ptr, int off)
{
  return ptr[off + 1];
}
