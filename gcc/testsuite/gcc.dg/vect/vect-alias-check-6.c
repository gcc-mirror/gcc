/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define N 16

struct s { int x[N]; };

void
f1 (struct s *a, struct s *b)
{
  for (int i = 0; i < N - 1; ++i)
    a->x[i + 1] += b->x[i];
}

/* { dg-final { scan-tree-dump {checking that [^\n]* and [^\n]* have different addresses} "vect" } } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
