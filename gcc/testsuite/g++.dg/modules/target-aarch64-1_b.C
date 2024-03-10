// PR c++/111224
// { dg-module-do link { target aarch64*-*-* } }
// { dg-require-effective-target aarch64_asm_sve_ok }
// { dg-additional-options "-fmodules-ts -fno-module-lazy -march=armv8.2-a+sve" }

#include <arm_sve.h>
import M;

int main() {
  svbool_t x = svptrue_b8 ();
  svfloat16_t f = svdup_n_f16(1.0);
  foo(x, f);
}
