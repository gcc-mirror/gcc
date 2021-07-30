// { dg-options "" }

struct A { };
struct B {
  struct: A { int i; };		// { dg-error "anonymous struct with base" }
};
union U {
  struct: A { int i; };		// { dg-error "anonymous struct with base" }
};
