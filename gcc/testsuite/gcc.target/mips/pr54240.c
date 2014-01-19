/* { dg-do compile } */
/* { dg-options "-fdump-tree-phiopt-details -ffat-lto-objects isa>=4" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef struct s {
  int v;
  int b;
  struct s *l;
  struct s *r;
} S;

/* Test requires conditional moves.  */
NOMIPS16 int foo(S *s)
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
