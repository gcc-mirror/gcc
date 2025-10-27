/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3" } */

long *print_bfloat_block;
void ftoastr(float);
void print_bfloat() {
  for (;;) {
    long j;
    union {
      _Float16 x;
      char b[]
    } u;
    j = 0;
    for (; j < sizeof 0; j++)
      u.b[j] = print_bfloat_block[j];
    ftoastr(u.x);
  }
}
