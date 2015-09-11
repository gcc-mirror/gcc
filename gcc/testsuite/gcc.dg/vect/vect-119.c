/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define OUTER 32
#define INNER 40

static unsigned int
bar (const unsigned int x[INNER][2], unsigned int sum)
{
  int i;

  for (i = 0; i < INNER; i++)
    sum += x[i][0] * x[i][0] + x[i][1] * x[i][1];
  return sum;
}

unsigned int foo (const unsigned int x[OUTER][INNER][2])
{
  int i;
  unsigned int sum;

  sum = 0.0f;
  for (i = 0; i < OUTER; i++)
    sum = bar (x[i], sum);
  return sum;
}

/* { dg-final { scan-tree-dump-times "Detected interleaving load of size 2" 1 "vect" } } */
