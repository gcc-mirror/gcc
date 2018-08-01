/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */

/* All four functions should be folded to:

   (sizetype) (a + 15 - b) < 30.  */

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

/* { dg-final { scan-tree-dump-times { = [^\n]* - [^\n]*;} 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times { = [^\n]* \+ [^\n]*;} 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times { = [^\n]*\ > [^\n]*;} 4 "optimized" } } */
/* { dg-final { scan-tree-dump-not {=[^\n]*\ < [^\n]*;} "optimized" } } */
/* { dg-final { scan-tree-dump-times { \+ 15} 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times { > 30} 4 "optimized" } } */
