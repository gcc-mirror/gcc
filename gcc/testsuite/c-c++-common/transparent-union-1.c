/* PR c++/51228 */
/* { dg-options "-Wno-c++-compat" { target c } } */

typedef union {} U __attribute__((transparent_union)); /* { dg-warning "ignored" } */

void foo(U u) {}
