/* { dg-do compile } */
/* { dg-options "-O3 -Wall" } */

void foo(float tmpCorr[9][9]);
float bar;

void finalDigits(int& n)
{
  float tmpCorr[9][9] = {{0}};

  foo(tmpCorr);
  for (int i = 0; i < n; i++) {
    for (int j = i+1; j < n; j++) {
      bar = tmpCorr[i][j];
    }
  }
}

