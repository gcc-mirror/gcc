/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

bool
foo(unsigned i)
{
  bool result = true;
  while (i)
    {
      i = i % 3;
      i = i - (i == 2 ? 2 : i ? 1 : 0);
      result = !result;
    }
  return result;
}

/* We should be able to eliminate the i - operation.  */
/* { dg-final { scan-tree-dump-not "i_.* - " "optimized" } } */
