/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -std=gnu23 -O2" { target { rv64 } } } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -std=gnu23 -O2" { target { rv32 } } } */

long print_halffloat_j;
int *print_halffloat_block;
void ftoastr(float);
enum { BFLOATING_POINTvoid } print_halffloat() {
  union {
    _Float16 x;
    char b[];
  } u;
  print_halffloat_j = 0;
  for (; print_halffloat_j < sizeof(_Float16); print_halffloat_j++)
    u.b[print_halffloat_j] = print_halffloat_block[print_halffloat_j];
  ftoastr(u.x);
}
