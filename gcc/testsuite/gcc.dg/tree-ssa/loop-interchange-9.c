/* { dg-do run } */
/* { dg-options "-O2 -floop-interchange -fdump-tree-linterchange-details" } */
/* { dg-require-effective-target size20plus } */
/* { dg-skip-if "too big data segment" { visium-*-* } } */

#define M 256
int a[M][M], b[M][M], c[M], d[M];
void __attribute__((noinline))
simple_reduc_1 (int n)
{
  for (int j = 0; j < n; j++)
    {
      int sum = c[j];
      for (int i = 0; i < n; i++)
	sum = sum + a[i][j]*b[i][j];

      c[j] = sum;
    }
}

void __attribute__((noinline))
simple_reduc_2 (int n)
{
  for (int j = 0; j < n; j++)
    {
      int sum = d[j];
      for (int i = 0; i < n; i++)
	sum = sum + a[i][j]*b[i][j];

      asm volatile ("" ::: "memory");
      d[j] = sum;
    }
}

extern void abort ();

static void __attribute__((noinline))
init (int i)
{
  c[i] = 0;
  d[i] = 0;
  for (int j = 0; j < M; j++)
    {
      a[i][j] = i;
      b[i][j] = j;
    }
}

int main (void)
{
  for (int i = 0; i < M; ++i)
    init (i);

  simple_reduc_1 (M);
  simple_reduc_2 (M);

  for (int i = 0; i < M; ++i)
    if (c[i] != d[i])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Loop_pair<outer:., inner:.> is interchanged" 1 "linterchange" } } */
