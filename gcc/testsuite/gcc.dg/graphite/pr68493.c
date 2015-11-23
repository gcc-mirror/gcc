/* { dg-options "-O1 -floop-nest-optimize" } */

int ce[2];
int o5;
int p7;

int foo (void)
{
  int j1;
  ce[0] = 0;
  for (j1 = 0; j1 < 2; ++j1)
    for (o5 = 1; o5 >= 0; --o5)
      p7 += ce[o5];
  return 0;
}

int du;

int bar (void)
{
  int u7[2];
  int ar;

  for (ar = 0; ar < 2; ++ar) {
    int xo;

    for (xo = 0; xo < 2; ++xo) {
      du += u7[ar];
      u7[0] = 0;
    }
  }

  return 0;
}
