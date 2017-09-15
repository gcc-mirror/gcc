/* { dg-do run } */

static double identity[4][4] = {{1, 0, 0, 0},
                                {0, 1, 0, 0},
                                {0, 0, 1, 0},
                                {0, 0, 0, 1}};
static double expected[4][4] = {{1, 0, 0, 0},
                                {0, 0, 0, 0},
                                {0, 0, 0, 0},
                                {0, 0, 0, 0}};

static void __attribute__((noinline,noclone))
kernel(double A[4][4])
{
  double tmp[4][4];
  for (int j = 0; j < 4; j++)
    for (int k = 0; k < 4; k++)
      tmp[j][k] = identity[j][0] * identity[j][k];
  for (int j = 0; j < 4; j++ )
    for (int k = 0; k < 4; k++)
      A[j][k] = tmp[j][k];
}

int main(void)
{
  double A[4][4] = {{0.0}};
  kernel(A);
  for ( int i = 0; i < 4; i++ )
    for ( int j = 0; j < 4; j++ )
      if (A[i][j] != expected[i][j])
	__builtin_abort ();
  return 0;
}
