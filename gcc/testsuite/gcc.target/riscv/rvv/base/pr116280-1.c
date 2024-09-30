/* Test there is no ICE when compile.  */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvbb -mabi=lp64d -O3" } */

short a;
char b;

void
test (int e[][1][1], char f[][1][1][1][1]) {
  for (int g; b;)
    for (;;)
      for (int h; h < 4073709551572ULL; h += 18446744073709551612U)
        a = f[2][2][1][4073709551612][1] << e[1][1][g];
}
