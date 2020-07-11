/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cselim-details" } */

typedef union {
  int i;
  float f;
} U;

int foo(U *u, int b, int i)
{
  u->i = 0;
  if (b)
    u->i = i;
  return u->i;
}

/* { dg-final { scan-tree-dump "Conditional store replacement" "cselim" } } */
