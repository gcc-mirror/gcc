/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl1024b -mabi=lp64d -mrvv-vector-bits=zvl -O3" } */

int a, c;
char b;
short d;
void e() {
  for (; d; d++) {
    for (; c;)
      ;
    b = 3;
    for (; b; b = 0)
      if (a)
        break;
  }
}
