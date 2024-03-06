/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32d" { target { rv32 } } } */

short a, c;
int b, d, i;
volatile char e;
static int f[] = {1, 1};
long g;
int volatile h;
short(j)() { return b ? a : 0; }
void k() {
l:
  h;
  g = 0;
  for (; g <= 2; g++) {
    d | ((i || j() & (0 == f[g])) ^ i) && e;
    if (c)
      goto l;
  }
}

