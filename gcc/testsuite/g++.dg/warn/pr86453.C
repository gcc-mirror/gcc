// { dg-do compile }
// { dg-additional-options "-flto" { target lto } }
struct X {
  int *__attribute__((aligned(2), packed)) a; // { dg-warning "ignoring attribute .packed. because it conflicts with attribute .aligned." }
} b;
