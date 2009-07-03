/* PR 31885 */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

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

/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
