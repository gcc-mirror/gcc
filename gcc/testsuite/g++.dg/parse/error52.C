// PR c++/12288

class X {};

struct S {
  explicit S (const X::T&) {}  // { dg-error "does not name a type" }
};

class Y {};

typedef Y::T xt;               // { dg-error "does not name a type" }
