// PR c++/13438
// {  dg-options "-fshow-column" }

struct A { friend typename struct B; };


// { dg-error "19:using 'typename' outside of template" "" { target *-*-* } 4 }
// { dg-error "28:expected nested-name-specifier before 'struct'" "" { target *-*-* } 4 }
// { dg-error "35:multiple types in one declaration" "" { target *-*-* } 4 }
// { dg-error "12:friend declaration does not name a class or function" "" { target *-*-* } 4 }
