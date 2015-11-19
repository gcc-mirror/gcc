/* { dg-options "-O2 -floop-nest-optimize" } */

int au[4] = { 0 };

int
main(void)
{
  int dc;
  int m7;
  int lv;
  int f2;
  int uq[3] = { 1 };
  for (dc = 0; dc < 2; ++dc) {
    for (lv = 0; lv < 2; ++lv)
      for (m7 = 0; m7 < 3; ++m7) {
        if (uq[dc] == 0)
          continue;
        for (f2 = 0; f2 < 3; ++f2)
          au[dc+2] = uq[f2];
      }
  }
  return 0;
}
