// PR c++/121553
// { dg-do compile { target c++11 } }
// { dg-options "-Wshadow" }

void foo () {
 int i;
  auto f = [i] () { int i; return 0; }; // { dg-warning "declaration of .i. shadows a lambda capture" }
}
