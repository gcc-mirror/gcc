// PR c++/119844
// { dg-additional-options "-fmodules" }

struct A; // { dg-message "declaration" }
struct B; // { dg-message "declaration" }
struct C; // { dg-message "declaration" }

template <typename T> struct D; // { dg-message "declaration" }
template <typename T> struct E; // { dg-message "declaration" }

template <typename T> struct F; // { dg-message "declaration" }

template <typename T> struct G { int value; };  // { dg-bogus "module" }
template <> struct G<int>; // { dg-message "declaration" }
// { dg-bogus "module" "" { target *-*-* } .-1 }

template <typename T> struct H { int value; };  // { dg-bogus "module" }
template <typename T> struct H<const T>;  // { dg-message "declaration" }
// { dg-bogus "module" "" { target *-*-* } .-1 }

struct MainWindow {
  A* a;
  B* b;
  C* c;

  D<int>* d;
  E<int>* e;
  F<int>* f;

  G<int>* g;
  H<const int>* h;
};

import M;

int foo(MainWindow m) {
  int result = 0;
  result += m.a->value; // { dg-error "incomplete" }
  result += m.b->value; // { dg-error "incomplete" }
  result += m.c->value; // { dg-error "incomplete" }
  result += m.d->value; // { dg-error "incomplete" }
  result += m.e->value; // { dg-error "incomplete" }
  result += m.f->value; // { dg-error "incomplete" }
  result += m.g->value; // { dg-error "incomplete" }
  result += m.h->value; // { dg-error "incomplete" }
  return result;
}

// { dg-message "A@M" "" { target *-*-* } 0 }
// { dg-bogus   "B@M" "" { target *-*-* } 0 }
// { dg-bogus   "C@M" "" { target *-*-* } 0 }
// { dg-message "D@M" "" { target *-*-* } 0 }
// { dg-bogus   "E@M" "" { target *-*-* } 0 }
// { dg-message "F@M" "" { target *-*-* } 0 }
// { dg-message "G@M" "" { target *-*-* } 0 }
// { dg-message "H@M" "" { target *-*-* } 0 }

