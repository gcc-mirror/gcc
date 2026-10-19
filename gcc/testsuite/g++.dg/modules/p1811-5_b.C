// { dg-additional-options "-fmodules" }

import M;

auto imported_a = make_a ();
struct A { };
struct A { };		      // { dg-error "redefinition" "" { xfail *-*-* } }
