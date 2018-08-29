/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details -fdump-tree-dse1-details" } */

int foo(unsigned char c, signed char d, int e)
{
  int res = 0;
  char x[256];
  __builtin_memset (x, c, 256);
  res += x[54];
  __builtin_memset (x, d, 256);
  res += x[54];
  __builtin_memset (x, e, 256);
  res += x[54];
  return res;
}

/* The loads get replaced with conversions from c or d and e.  */
/* { dg-final { scan-tree-dump-times "Inserted" 2 "fre1" } } */
/* { dg-final { scan-tree-dump-times "Replaced x" 3 "fre1" } } */
/* And the memsets removed as dead.  */
/* { dg-final { scan-tree-dump-times "Deleted dead call" 3 "dse1" } } */
