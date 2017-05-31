/* { dg-do compile }  */
/* { dg-options "-O2 -fdump-rtl-combine" }  */

int a;

double b1() {
  int c = a << 1;
  return 1 - c;
}

double b2() {
  int c = a << 2;
  return 1 - c;
}

double b3() {
  int c = a << 3;
  return 1 - c;
}

/* { dg-final { scan-rtl-dump-times "\\*sub_n" 3 "combine" } } */
