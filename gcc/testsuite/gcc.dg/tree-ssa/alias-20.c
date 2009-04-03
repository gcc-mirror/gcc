/* { dg-do compile } */
/* { dg-options "-O -fstrict-aliasing -fdump-tree-optimized" } */

struct S { float f; int i; };
struct R { int x; int i; };

/* Strict-aliasing rules say that int and float do not alias.  */
int bar(struct S *s, int *i)
{
  *i = 0;
  s->f = 1.0;
  return *i;
}

/* Strict-aliasing rules say that S and R do not alias.  */
int foo(struct S *s, struct R *r)
{
  r->i = 0;
  s->i = 1;
  return r->i;
}

/* { dg-final { scan-tree-dump-times "return 0;" 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
