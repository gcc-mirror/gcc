/* PR middle-end/37248 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct S
{
  unsigned char a : 1;
  unsigned char b : 1;
  unsigned char c : 1;
} s;

int
foo (struct S x)
{
  return x.a && x.b && x.c;
}

/* { dg-final { scan-tree-dump "& 7;" "optimized" } } */
/* { dg-final { scan-tree-dump "== 7;" "optimized" } } */
