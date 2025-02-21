/* { dg-options "-ftree-slp-vectorize -ffp-contract=on -ffloat-store"  }  */

_Complex foo_x_0;
int foo_n11, foo_i, foo_l;
_Complex float foo_s;
_Complex *foo_f_0_0_0;

void
foo() {
  _Complex f[foo_l];
  for (; foo_i; foo_i++) {
    int n9;
    for (; n9 < foo_l; n9++)
      for (; foo_n11; foo_n11++)
        foo_s += foo_f_0_0_0[n9] * 0 * foo_f_0_0_0[foo_n11];
    foo_x_0 += foo_s;
  }
}
