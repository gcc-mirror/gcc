/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=zero -fdump-tree-ssa" } */

int a;
int foo (void);
int bar (void);

void
baz (void)
{
  int *b[6];
  if (foo ())
    a |= bar ();
}

/* { dg-final { scan-tree-dump-not "DEFERRED_INIT" "ssa" } } */
