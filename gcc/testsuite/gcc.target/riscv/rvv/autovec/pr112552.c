/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax -w -Wno-incompatible-pointer-types" } */

int a, c, d;
void (*b)();
void (*e)();
void g();

void h() {
  for (; a; --a) {
    char *f = h;
    e = b || g > 1 ? g : b;
    d |= !e;
    *f ^= c;
  }
}
