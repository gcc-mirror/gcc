/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define N 16

struct s { int x[N]; };

void
f1 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a->x[i] += b->x[N - i - 1];
}

/* { dg-final { scan-tree-dump {checking that [^\n]* and [^\n]* have different addresses} "vect" } } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { vect_perm && vect_element_align } } } } */
