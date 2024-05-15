/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

int a;
int foo (void);
int bar (void);

void
baz (void)
{
  int *b[6];
  b[0] = &a;
  if (foo ())
    a |= bar ();
}

/* { dg-final { scan-tree-dump "Deleted dead store: b\\\[0\\\] = &a;" "dse1" } } */
