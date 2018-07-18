/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

/* Intended to be larger than any VF.  */
#define GAP 128
#define N (GAP * 3)

struct s { int x[N]; };

void
f1 (struct s *a, struct s *b)
{
  for (int i = 0; i < GAP * 2; ++i)
    a->x[i + GAP] += b->x[i];
}

/* { dg-final { scan-tree-dump-times "consider run-time aliasing" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "improved number of alias checks from 1 to 0" 1 "vect" { xfail vect_variable_length } } } */
/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" } } */
