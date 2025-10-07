/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mavx" { target { x86_64-*-* i?86-*-* } } } */


int a, b;

void f(float g[][5]) {
  int c;

  for (c = 0; c != a; c++)
    g[1][c] = c;

  for (int d; d; d++)
    for (int e = 1; e != b; e++) {
      for (c = 0; c != a; c++) {
        g[0][1] = 1;

        if (g[1][c])
          g[1][c] = 1;
      }
    }
}
