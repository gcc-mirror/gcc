// PRMS Id: 5155

struct A {
  enum foo { bar };
};

typedef A::foo A::foo;		// ERROR - causes compiler segfault
