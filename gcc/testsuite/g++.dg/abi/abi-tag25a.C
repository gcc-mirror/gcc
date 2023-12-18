// PR c++/109715
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fabi-version=18 -fabi-compat-version=18 -Wabi=0" }

#include "abi-tag25.C"

// { dg-warning "mangled name" "" { target *-*-* } 5 }
// { dg-warning "mangled name" "" { target c++14 } 11 }

// { dg-final { scan-assembler "_Z3funIiEvv" } }
// { dg-final { scan-assembler "_Z3varIiE" { target c++14 } } }
