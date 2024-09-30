/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ssa -std=gnu11" } */
/* { dg-final { scan-tree-dump-times "__builtin_unreachable" 4 "ssa" } } */

void bar1 (void);
void bar2 (void);
void bar3 (void);
void bar4 (void);

_Noreturn void
foo1 (int *p, int y)
{
  bar1 ();
  *p = y;
  return;	/* { dg-warning "function declared 'noreturn' has a 'return' statement" } */
}		/* { dg-warning "'noreturn' function does return" "" { target *-*-* } .-1 } */

_Noreturn void
foo2 (int *p, int y)
{
  bar2 ();
  *p = y;
}		/* { dg-warning "'noreturn' function does return" } */

_Noreturn void
foo3 (int *p, int y)
{
  if (y > 10)
    return;	/* { dg-warning "function declared 'noreturn' has a 'return' statement" } */
  bar3 ();
  *p = y;
  return;	/* { dg-warning "function declared 'noreturn' has a 'return' statement" } */
}		/* { dg-warning "'noreturn' function does return" } */

_Noreturn void
foo4 (int *p, int y)
{
  if (y > 10)
    return;	/* { dg-warning "function declared 'noreturn' has a 'return' statement" } */
  bar4 ();
  *p = y;
}		/* { dg-warning "'noreturn' function does return" } */
