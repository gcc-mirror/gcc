/* { dg-do compile } */
/* { dg-options "-O2 -misel -fdump-tree-phiopt-details" } */

typedef struct s {
  int v;
  int b;
  struct s *l;
  struct s *r;
} S;


int foo(S *s)
{
  S *this;
  S *next;

  this = s;
  if (this->b)
    next = this->l;
  else
    next = this->r;

  return next->v;
}

/* { dg-final { scan-tree-dump "Hoisting adjacent loads" "phiopt1" } } */
/* { dg-final { cleanup-tree-dump "phiopt1" } } */
