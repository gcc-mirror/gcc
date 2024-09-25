/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize --param=aarch64-autovec-preference=prefer-asimd" } */

extern short d[], e[];
void f(char a, long *b) {
  for (int c = 0; c < a - 12; c++) {
    d[c] = b[c];
    e[c] = 0;
  }
}
