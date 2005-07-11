/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-vars" } */

int bar (void);

void foo (void)
{
  unsigned i, j, n;

  for (i = 0; i < 100000; i++)
    ;

  n = bar ();
  for (i = 0; i < n; i++)
    ;

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      ;

  /* These should not be removed.  */
  for (i = 0; i < 10000; i++)
    bar ();

  for (i = 0; i != n; i += 2)
    ;
}

/* { dg-final { scan-tree-dump-times "if " 3 "vars" } } */
/* { dg-final { scan-tree-dump-times "bar " 2 "vars" } } */

/* { dg-final { cleanup-tree-dump "vars" } } */
