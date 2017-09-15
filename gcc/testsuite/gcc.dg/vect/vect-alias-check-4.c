/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-version-for-alias-checks=0" } */

#define N 16

struct s1 { int a[N]; };
struct s2 { struct s1 b; int c; };
struct s3 { int d; struct s1 e; };
union u { struct s2 f; struct s3 g; };

/* We allow a and b to overlap arbitrarily.  */

void
f1 (int a[][N], int b[][N])
{
  for (int i = 0; i < N; ++i)
    a[0][i] += b[0][i];
}

void
f2 (union u *a, union u *b)
{
  for (int i = 0; i < N; ++i)
    a->f.b.a[i] += b->g.e.a[i];
}

void
f3 (struct s1 *a, struct s1 *b)
{
  for (int i = 0; i < N - 1; ++i)
    a->a[i + 1] += b->a[i];
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
