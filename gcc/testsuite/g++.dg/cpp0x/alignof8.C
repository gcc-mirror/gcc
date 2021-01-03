// PR c++/88115
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-pedantic -fabi-version=14 -Wabi" }

#include "alignof7.C"

// { dg-warning "changes between" "" { target *-*-* } 11 }
// { dg-warning "changes between" "" { target *-*-* } 12 }

// { dg-final { scan-assembler "_Z2f1IiEvDTatT_E" } }
// { dg-final { scan-assembler "_Z2f2IiEvDTaztlT_EE" } }
// { dg-final { scan-assembler "_Z2f3IiEvDTatT_E" } }
// { dg-final { scan-assembler "_Z2f4IiEvDTaztlT_EE" } }
