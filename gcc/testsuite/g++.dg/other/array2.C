// PR c++/25263
// { dg-do compile }

int x[1/0];  // { dg-warning "division by zero" }
             // { dg-error "constant" "constant" { target *-*-* } 4 }

