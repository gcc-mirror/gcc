// PR target/99813
/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 -march=armv8.2-a+sve -fvect-cost-model=unlimited -fno-tree-dominator-opts -mtune=cortex-a72" } */

long a, b;
bool c[2][14][2][16], f[2][14][2][16];
bool d;
char e[2][4][2][6];
void g() {
  a = 0;
  for (int h = 0; h < 2; ++h)
    for (int i = 0; i < 14; ++i)
      for (int j = 0; j < 2; ++j)
        for (int k = 0; k < 16; ++k)
          c[h][i][j][k] = 0;
  d = 0;
  for (int h; h < 2; ++h)
    for (int i = 0; i < 4; ++i)
      for (int j = 0; j < 2; ++j)
        for (int k = 0; k < 6; ++k)
          e[h][i][j][k] = 6;
  for (int h = 0; h < 2; ++h)
    for (int i = 0; i < 14; ++i)
      for (int j = 0; j < 2; ++j)
        for (int k = 0; k < 16; ++k)
          f[h][i][j][k] = b = 9;
}
