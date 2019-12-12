/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fwrapv-pointer -fdump-tree-optimized" } */

_Bool
f1 (char *a, char *b)
{
  return (a + 16 <= b) || (b + 16 <= a);
}

_Bool
f2 (char *a, char *b)
{
  return (a + 15 < b) || (b + 15 < a);
}

_Bool
f3 (char *a, char *b)
{
  return (a + 16 <= b) | (b + 16 <= a);
}

_Bool
f4 (char *a, char *b)
{
  return (a + 15 < b) | (b + 15 < a);
}

/* { dg-final { scan-tree-dump-not { = [^\n]* - [^\n]*;} "optimized" } } */
/* { dg-final { scan-tree-dump-times { = [^\n]* \+ [^\n]*;} 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times { \+ 15} 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times { \+ 16} 4 "optimized" } } */
