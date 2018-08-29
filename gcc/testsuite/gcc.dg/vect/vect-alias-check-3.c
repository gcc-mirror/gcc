/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-version-for-alias-checks=0 -fopenmp-simd" } */

/* Intended to be larger than any VF.  */
#define GAP 128
#define N (GAP * 3)

struct s { int x[N + 1]; };
struct t { struct s x[N + 1]; };
struct u { int x[N + 1]; int y; };
struct v { struct s s; };

void
f1 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a->x[i] += b->x[i];
}

void
f2 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a[1].x[i] += b[2].x[i];
}

void
f3 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a[1].x[i] += b[i].x[i];
}

void
f4 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a[i].x[i] += b[i].x[i];
}

void
f5 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a->x[i] += b->x[i + 1];
}

void
f6 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a[1].x[i] += b[2].x[i + 1];
}

void
f7 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a[1].x[i] += b[i].x[i + 1];
}

void
f8 (struct s *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a[i].x[i] += b[i].x[i + 1];
}

void
f9 (struct s *a, struct t *b)
{
  for (int i = 0; i < N; ++i)
    a->x[i] += b->x[1].x[i];
}

void
f10 (struct s *a, struct t *b)
{
  for (int i = 0; i < N; ++i)
    a->x[i] += b->x[i].x[i];
}

void
f11 (struct u *a, struct u *b)
{
  for (int i = 0; i < N; ++i)
    a->x[i] += b->x[i] + b[i].y;
}

void
f12 (struct s *a, struct s *b)
{
  for (int i = 0; i < GAP; ++i)
    a->x[i + GAP] += b->x[i];
}

void
f13 (struct s *a, struct s *b)
{
  for (int i = 0; i < GAP * 2; ++i)
    a->x[i + GAP] += b->x[i];
}

void
f14 (struct v *a, struct s *b)
{
  for (int i = 0; i < N; ++i)
    a->s.x[i] = b->x[i];
}

void
f15 (struct s *a, struct s *b)
{
  #pragma omp simd safelen(N)
  for (int i = 0; i < N; ++i)
    a->x[i + 1] += b->x[i];
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 15 "vect" } } */
