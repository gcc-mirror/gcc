/* { dg-additional-options "-Wno-analyzer-too-complex" } */

int foo ()
{
  static volatile int v = 42;
  int __result_foo;

  __result_foo = (int) v;
  return __result_foo;
}

void test (int * restrict n, int * restrict flag)
{
  int i;
  int j;
  int k;
  double t;
  int tt;
  double v;

  if (*flag)
    {
      t = 4.2e+1;
      tt = foo ();
    }
  L_1: ;
  v = 0.0;
  {
    int D_3353;

    D_3353 = *n;
    i = 1;
    if (i <= D_3353)
      {
        while (1)
          {
            {
              int D_3369;

              v = 0.0;
              if (*flag)
                {
                  if (tt == i)
                    {
                      {
                        double M_0;

                        M_0 = v;
                        if (t > M_0 || (int) (M_0 != M_0))
                          {
                            M_0 = t;
                          }
                        v = M_0;
                      }
                    }
                  L_5:;
                }
              L_4:;
              {
                int D_3359;

                D_3359 = *n;
                j = 1;
                if (j <= D_3359)
                  {
                    while (1)
                      {
                        {
                          int D_3368;

                          {
                            int D_3362;

                            D_3362 = *n;
                            k = 1;
                            if (k <= D_3362)
                              {
                                while (1)
                                  {
                                    {
                                      int D_3367;

                                      {
                                        double D_3366;
                                        double M_1;

                                        M_1 = v;
                                        D_3366 = (double) __builtin_sinf ((float) (j * k));
                                        if (D_3366 > M_1 || (int) (M_1 != M_1))
                                          {
                                            M_1 = D_3366;
                                          }
                                        v = M_1;
                                      }
                                      L_8:;
                                      D_3367 = k == D_3362;
                                      k = k + 1;
                                      if (D_3367) goto L_9;
                                    }
                                  }
                              }
                            L_9:;
                          }
                          L_6:;
                          D_3368 = j == D_3359;
                          j = j + 1;
                          if (D_3368) goto L_7;
                        }
                      }
                  }
                L_7:;
              }
              L_2:;
              D_3369 = i == D_3353;
              i = i + 1;
              if (D_3369) goto L_3;
            }
          }
      }
    L_3:;
  }
}


int main ()
{
  int flag;
  int n;

  n = 4;
  flag = 0;
  test (&n, &flag);
  return 0;
}
