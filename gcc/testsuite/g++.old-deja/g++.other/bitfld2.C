// { dg-do assemble  }
// { dg-options "-funsigned-bitfields" }
// Origin: Mark Mitchell <mark@codesourcery.com>

typedef int i[4];

struct S {
  i j:12; // { dg-error "" } array type as bitfield
};
