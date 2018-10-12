// { dg-options "-fmodules-atom" }

export module thing;
int i;
import baz; // { dg-error "must be within module preamble" }

// { dg-warning "not exporting module" "" { target *-*-* } 0 }
