/* PR 31885 */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-empty" } */

struct s {
    int *blah;
};

static struct s array[] = { { 0 } };

void
foo (struct s *p)
{
  struct s *q = &array[1];
  while (p < q)
    p++;
}

/* { dg-final { scan-tree-dump-times "Removing empty loop" 1 "empty" } } */
/* { dg-final { cleanup-tree-dump "empty" } } */
