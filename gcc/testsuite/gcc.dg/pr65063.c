/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-loop-ivcanon -fno-tree-vectorize" } */

static int in[8][4];
static int out[4];
static const int check_result[] = {0, 16, 256, 4096};

static inline void foo ()
{
  int sum;
  int i, j, k;
  for (k = 0; k < 4; k++)
    {
      sum = 1;
      for (j = 0; j < 4; j++)
	for (i = 0; i < 4; i++)
	  sum *= in[i + k][j];
      out[k] = sum;
    }
}

int main ()
{
  int i, j, k;
  for (i = 0; i < 8; i++)
    for (j = 0; j < 4; j++)
      in[i][j] = (i + 2) / 3;
  foo ();
  for (k = 0; k < 4; k++)
    if (out[k] != check_result[k])
      __builtin_abort ();
  return 0;
}
