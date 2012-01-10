/* PR c++/51228 */

typedef union {} U __attribute__((transparent_union)); /* { dg-warning "ignored" } */

void foo(U u) {}
