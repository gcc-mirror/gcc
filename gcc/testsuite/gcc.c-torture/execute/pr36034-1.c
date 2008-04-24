double x[5][10] = { { 10, 11, 12, 13, 14, 15, -1, -1, -1, -1 },
                    { 21, 22, 23, 24, 25, 26, -1, -1, -1, -1 },
                    { 32, 33, 34, 35, 36, 37, -1, -1, -1, -1 },
                    { 43, 44, 45, 46, 47, 48, -1, -1, -1, -1 },
                    { 54, 55, 56, 57, 58, 59, -1, -1, -1, -1 } };
double tmp[5][6];

void __attribute__((noinline))
test (void)
{ 
  int i, j;
  for (i = 0; i < 5; ++i)
    {
      tmp[i][0] = x[i][0];
      tmp[i][1] = x[i][1];
      tmp[i][2] = x[i][2];
      tmp[i][3] = x[i][3];
      tmp[i][4] = x[i][4];
      tmp[i][5] = x[i][5];
    }
}
extern void abort (void);
int main()
{
  int i, j;
  test();
  for (i = 0; i < 5; ++i)
    for (j = 0; j < 6; ++j)
      if (tmp[i][j] == -1)
        abort ();
  return 0;
}
