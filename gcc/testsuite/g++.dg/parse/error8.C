// PR c++/13438
// {  dg-options "-fshow-column" }

struct A { friend typename struct B; };


// { dg-error "19: error: using 'typename' outside of template" "" { target *-*-* } { 4 } }
// { dg-error "28: error: expected nested-name-specifier before 'struct'" "" { target *-*-* } { 4 } }
// { dg-error "35: error: multiple types in one declaration" "" { target *-*-* } { 4 } }
// { dg-error "12: error: friend declaration does not name a class or function" "" { target *-*-* } { 4 } }
