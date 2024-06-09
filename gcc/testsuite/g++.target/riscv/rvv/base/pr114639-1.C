/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

typedef long c;

#pragma riscv intrinsic "vector"

template <unsigned long> struct d {};

struct e {
  using f = d<0>;
};

struct g {
  using f = e::f;
};

template <typename, int> using h = g::f;
template <unsigned long i> long get_vl (d<i>);

vbool16_t test (vuint64m4_t a) {
  c b;
  return __riscv_vmsne_vx_u64m4_b16(a, b, get_vl (h<c, 2>()));
}
