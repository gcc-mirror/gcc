/* PR middle-end/90694 - incorrect representation of ADDR_EXPR involving
   a pointer to array
   { dg-do compile }
   { dg-options "-fdump-tree-original" } */

typedef char A8[8];

unsigned f (A8 *pa)
{
  return __builtin_strlen (&(*pa)[2]);
}

/* Veriy the expression is correct in the dump:
  { dg-final { scan-tree-dump-not "\\\&\\\*pa\\\[2\\\]" "original" } }
  { dg-final { scan-tree-dump "\\\&\\\(\\\*pa\\\)\\\[2\\\]" "original" } } */
