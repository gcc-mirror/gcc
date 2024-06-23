/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f" { target { rv32 } } } */

int x[5];

void
d(int a, int b, int c) {
  for (int i = 0; i < 5; i++)
    x[i] = (a != b) ? c : a;
}
