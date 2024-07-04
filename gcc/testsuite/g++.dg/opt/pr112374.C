// PR tree-optimization/112374
// { dg-do compile { target c++11 } }
// { dg-options "-fcompare-debug -gno-statement-frontiers -O2" }
// { dg-additional-options "-march=skylake-avx512" { target i?86-*-* x86_64-*-* } }
// { dg-additional-options "-march=armv9-a" { target aarch64*-*-* } }

struct t
{
  long coef[1];
  t(const unsigned long &a) : coef{(long)a} {};
  t(const t &a);
};
extern void gen_int_mode(t, int);
struct expand_vec_perm_d {
  unsigned char perm[64];
  int vmode;
  unsigned char nelt;
};
void expand_vec_perm_blend(struct expand_vec_perm_d *d) {
  unsigned long mask = 0;
  for (unsigned i = 0; i < 4; ++i)
    mask |= (d->perm[i] >= 4 ? 3 : 0) << (i * 2);
  gen_int_mode(mask, 0);
}
