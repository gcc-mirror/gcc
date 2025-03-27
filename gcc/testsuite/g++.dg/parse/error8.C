// PR c++/13438
// { dg-options "-fshow-column" }

struct A { friend typename struct B; };


// { dg-error "28:expected nested-name-specifier before 'struct'" "expected" { target *-*-* } 4 }
// { dg-error "19:multiple types in one declaration" "multiple" { target *-*-* } 4 }
// { dg-error "12:friend declaration does not name a class or function" "friend decl" { target *-*-* } 4 }
