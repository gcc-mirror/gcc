/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target size24plus } */

int var_2 = -2013646301;
int var_3 = -1126567434;
unsigned int var_12 = 1;
unsigned int var_19;
unsigned int arr_25 [24] [21] [15] [17] [15] ;

void __attribute__((noipa)) test()
{
  for (int a = 0; a < 3; a = 42)
    for (int b = 0; b < 20; b++)
      for (int c = 0; c < 4; c = 4)
        for (int d = 0; d < 6; d += 4)
          for (int e = 0; e < 4; e += 2) {
            arr_25[a][b][c][d][e] = var_2 || var_3;
            var_19 = var_12;
          }
}

int main()
{
    test();
    if (var_19 != 1)
      __builtin_abort ();
    return 0;
}
