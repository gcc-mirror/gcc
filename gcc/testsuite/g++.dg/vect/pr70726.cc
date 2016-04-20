// { dg-do compile }
// { dg-additional-options "-Ofast" }
// { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } }

extern long a;
extern int b[100];
extern unsigned c[5][5][2][4][2][2][3];
void fn1() {
  for (int d = 0; d < 2; d = d + 1)
    for (int e = 0; e < 5; e = e + 1)
      for (int f = 0; f < 3; f = f + 1)
        for (int g = 0; g < 3; g = g + 1)
          for (int h = 0; h < 2; h = h + 1)
            for (int i = 0; i < 4; i = i + 1)
              for (int j = 0; j < 2; j = j + 1)
                for (int k = 0; k < 2; k = k + 1)
                  for (int l = 0; l < 3; l = l + 1)
                    c[d][e][h][i][j][k][l] = a << b[f * 5 + g] + 4;
}
