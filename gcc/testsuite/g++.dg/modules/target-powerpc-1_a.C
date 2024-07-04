// PR c++/98645
// { dg-do compile { target powerpc*-*-* } }
// { dg-require-effective-target ppc_float128_sw }
// { dg-additional-options "-fmodules-ts -mfloat128 -mabi=ieeelongdouble -Wno-psabi" }

export module M;
export __ibm128 i = 0.0;
