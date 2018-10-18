// { dg-additional-options -Wno-pedantic }

export module foo;
// { dg-module-bmi !foo }
;

#pragma pack(2)
import baz; // { dg-error "must be within preamble" }

int i;

// { dg-warning "not exporting" "" { target *-*-* } 0 }
