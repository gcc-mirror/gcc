// { dg-do assemble  }
// PRMS Id: 5155

struct A {
  enum foo { bar };
};

typedef A::foo A::foo;		// { dg-error "16:typedef name" } causes compiler segfault
