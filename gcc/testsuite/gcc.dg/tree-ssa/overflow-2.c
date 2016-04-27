/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

int carry;
int f(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_sub_overflow(a, b, &r);
  return r > a;
}
int g(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_sub_overflow(a, b, &r);
  return a < r;
}
int h(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_sub_overflow(a, b, &r);
  return r <= a;
}
int i(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_sub_overflow(a, b, &r);
  return a >= r;
}
int j(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_add_overflow(a, b, &r);
  return r < a;
}
int j2(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_add_overflow(a, b, &r);
  return r < b;
}
int k(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_add_overflow(a, b, &r);
  return a > r;
}
int k2(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_add_overflow(a, b, &r);
  return b > r;
}
int l(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_add_overflow(a, b, &r);
  return r >= a;
}
int l2(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_add_overflow(a, b, &r);
  return r >= b;
}
int m(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_add_overflow(a, b, &r);
  return a <= r;
}
int m2(unsigned a, unsigned b) {
  unsigned r;
  carry = __builtin_add_overflow(a, b, &r);
  return b <= r;
}

/* { dg-final { scan-tree-dump-not "(le|lt|ge|gt)_expr" "optimized" } } */
/* { dg-final { scan-tree-dump-times "ADD_OVERFLOW" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "SUB_OVERFLOW" 4 "optimized" } } */
