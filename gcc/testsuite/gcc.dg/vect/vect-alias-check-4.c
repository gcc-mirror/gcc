/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-version-for-alias-checks=0 -fopt-info-vec-all" } */

#define N 16

struct s1 { int a[N]; };
struct s2 { struct s1 b; int c; };
struct s3 { int d; struct s1 e; };
union u { struct s2 f; struct s3 g; };

/* We allow a and b to overlap arbitrarily.  */

void
f1 (int a[][N], int b[][N]) /* { dg-message "vectorized 0 loops in function" } */
{
  for (int i = 0; i < N; ++i) /* { dg-missed "couldn't vectorize loop" } */
    a[0][i] += b[0][i];
  /* { dg-message "will not create alias checks, as --param vect-max-version-for-alias-checks == 0" "" { target *-*-* } .-2 } */
}

void
f2 (union u *a, union u *b) /* { dg-message "vectorized 0 loops in function" } */
{
  for (int i = 0; i < N; ++i) /* { dg-missed "couldn't vectorize loop" } */
    a->f.b.a[i] += b->g.e.a[i];
  /* { dg-message "will not create alias checks, as --param vect-max-version-for-alias-checks == 0" "" { target *-*-* } .-2 } */
}

void
f3 (struct s1 *a, struct s1 *b) /* { dg-message "vectorized 0 loops in function" } */
{
  for (int i = 0; i < N - 1; ++i) /* { dg-missed "couldn't vectorize loop" } */
    a->a[i + 1] += b->a[i]; /* { dg-missed "possible dependence between data-refs" } */
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
