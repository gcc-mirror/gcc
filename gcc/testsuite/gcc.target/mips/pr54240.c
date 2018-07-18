/* { dg-do compile } */
/* { dg-options "-fdump-tree-phiopt-details -ffat-lto-objects isa>=4" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */
/* This is testing for errors which can only happen in assembly generation.
   dg-error does not guarantee assembly generation, so we need to do it
   manually by using -ffat-lto-objects.  */

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
