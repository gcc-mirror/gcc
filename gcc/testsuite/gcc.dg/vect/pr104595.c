/* { dg-do compile } */
/* { dg-require-effective-target vect_condition } */

#define N 256
typedef char T;
extern T a[N];
extern T b[N];
extern T c[N];
extern _Bool pb[N];
extern char pc[N];

void predicate_by_bool()
{
  for (int i = 0; i < N; i++)
    c[i] = pb[i] ? a[i] : b[i];
}

void predicate_by_char()
{
  for (int i = 0; i < N; i++)
    c[i] = pc[i] ? a[i] : b[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
