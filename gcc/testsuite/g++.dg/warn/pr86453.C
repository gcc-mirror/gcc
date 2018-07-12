// { dg-do compile }
// { dg-additional-options "-flto" { target lto } }
struct X {
  int *__attribute__((aligned(2), packed)) a; // { dg-warning "attribute ignored" }
} b;
