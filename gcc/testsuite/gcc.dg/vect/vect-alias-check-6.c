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

void
f2 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a->x[i] += b->x[N - i - 1];
}

/* { dg-final { scan-tree-dump-times {checking that [^\n]* and [^\n]* have different addresses} 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" } } */
