/* PR target/70728 */
/* { dg-do compile } */
/* { dg-options "-S -Ofast -march=knl" } */

short a = -15726;
int b = (int)-7003557328690506537LL;
short c[5][5][3][6];
char d[2][5][3][2][4];
void fn1() {
  for (int e = 0; e < 3; e = e + 1)
    for (int f = 0; f < 2; f = f + 1)
      for (int g = 0; g < 4; g = g + 1)
        for (int h = 0; h < 3; h = h + 1)
          for (int i = 0; i < 2; i = i + 1)
            for (int j = 0; j < 4; j = j + 1)
              d[f][g][h][i][j] =
                  7 << (1236110361944357083 >> a + 15728) - 309027590486089270 >>
                  (c[e][f][h][j] + 2147483647 << ~b - 7003557328690506536) -
                      2147480981;
}
int main() {
  for (int k = 0; k < 5; ++k)
    for (int l = 0; l < 5; ++l)
      for (int m = 0; m < 3; ++m)
        for (int n = 0; n < 4; ++n)
          c[k][l][m][n] = -2639;
  fn1();
}

/* { dg-final { scan-assembler-not "sll\[ \\t\]+\[^\n\]*%\.mm(?:1\[6-9\]|\[2-3\]\[0-9\])" } } */
