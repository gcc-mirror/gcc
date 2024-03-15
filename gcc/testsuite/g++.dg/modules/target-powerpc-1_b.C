// PR c++/98645
// { dg-module-do compile { target powerpc*-*-* } }
// { dg-require-effective-target ppc_float128_sw }
// { dg-additional-options "-fmodules-ts -mfloat128 -mabi=ieeelongdouble -Wno-psabi" }

import M;

int main() {
  __ibm128 j = i;
}
