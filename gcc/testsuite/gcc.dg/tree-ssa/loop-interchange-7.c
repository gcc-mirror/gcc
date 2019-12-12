/* { dg-do run } */
/* { dg-options "-O2 -floop-interchange -fdump-tree-linterchange-details" } */
/* { dg-require-effective-target size20plus } */
/* { dg-skip-if "too big data segment" { visium-*-* } } */

#define M 256
int a[M][M], b[M][M], c[M][M], d[M][M];
void __attribute__((noinline))
matrix_mul_1 (int n)
{
      for (int k = 0; k < n; k++)
    for (int j = 0; j < n; j++)
  for (int i = 0; i < n; i++)
        c[i][j] = c[i][j] + a[i][k]*b[k][j];
}

void __attribute__((noinline))
matrix_mul_2 (int n)
{
  for (int i = 0; i < n; i++)
    {
      for (int j = 0; j < n; j++)
	{
	  for (int k = 0; k < n; k++)
	    d[i][j] = d[i][j] + a[i][k]*b[k][j];

	  asm volatile ("" ::: "memory");
	}
      asm volatile ("" ::: "memory");
    }
}

extern void abort ();

static void __attribute__((noinline))
init (int i)
{
  for (int j = 0; j < M; j++)
    {
      a[i][j] = i;
      b[i][j] = j;
      c[i][j] = 0;
      d[i][j] = 0;
    }
}

static int __attribute__((noinline))
check (int i)
{
  for (int j = 0; j < M; j++)
    if (c[i][j] != d[i][j])
      return 0;

  return 1;
}

int main (void)
{
  for (int i = 0; i < M; ++i)
    init (i);

  matrix_mul_1 (M);
  matrix_mul_2 (M);

  for (int i = 0; i < M; ++i)
    if (!check (i))
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Loop_pair<outer:., inner:.> is interchanged" 2 "linterchange" } } */
