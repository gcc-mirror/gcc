/* PR middle-end/103642 */
/* { dg-do compile } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

typedef struct
{
  int *a;
} S;

typedef struct
{
  S *s;
  int *ptr;
} T;

#define N 10

int main (void)
{
  T t;
  t.s = (S *) malloc (sizeof (S));
  t.s->a = (int *) malloc (sizeof(int) * N);

  #pragma omp target map(from: t.s->a[:N])
  {
    t.s->a[0] = 1;
  }

  free (t.s->a);
  free (t.s);

  return 0;
}
