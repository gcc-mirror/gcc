/* { dg-do compile } */
/* { dg-options "-O2 -fmath-errno -fdump-tree-cdce-details -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "cdce3.c:9: \[^\n\r]* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump "sqrtf \\(\[^\n\r]*\\); \\\[tail call\\\]" "optimized" } } */

float sqrtf (float);
float foo (float x)
{
  return sqrtf (x);
}

