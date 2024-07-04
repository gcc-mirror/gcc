/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl1024b -mabi=ilp32d -mrvv-vector-bits=zvl" } */

short a, b;
void c(int d) {
  for (; a; a--) {
    b = 0;
    for (; b <= 8; b++)
      if (d)
        break;
  }
}
