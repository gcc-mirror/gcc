// PR c++/111224
// { dg-do compile { target aarch64*-*-* } }
// { dg-require-effective-target aarch64_asm_sve_ok }
// { dg-additional-options "-fmodules-ts -march=armv8.2-a+sve" }

module;

// We can't do a header unit of this right now because this
// uses target attributes, that we don't yet support.
// See also PR c++/108080.
#include <arm_sve.h>

export module M;

export inline void foo(svbool_t x, svfloat16_t f) {
  svabs_f16_x(x, f);
}
