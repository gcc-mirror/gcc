/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */
struct A {}; /* { dg-warning "empty struct has size 0 in C" } */
union B {}; /* { dg-warning "empty union has size 0 in C" } */
struct C { struct D {}; int x; }; /* { dg-warning "empty struct has size 0 in C|declaration does not declare anything" } */
struct E { union F {}; int x; }; /* { dg-warning "empty union has size 0 in C|declaration does not declare anything" } */
union G { union H {}; int x; }; /* { dg-warning "empty union has size 0 in C|declaration does not declare anything" } */
union I { struct J {}; int x; }; /* { dg-warning "empty struct has size 0 in C|declaration does not declare anything" } */
