// { dg-options "-Wabi" }

struct E1 {};
struct E2 : public E1 {}; // { dg-warning "layout" }
struct E : public E1, public E2 {}; // { dg-warning "layout|ambiguity" }
struct N : public E { virtual void f () {} };  // { dg-warning "nearly" }
