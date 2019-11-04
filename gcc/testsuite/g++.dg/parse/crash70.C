// PR c++/92024
// { dg-additional-options "-Wshadow=compatible-local" }

template<typename>
struct S {
  S () {
    struct c;
      {
	struct c {}; // { dg-warning "Wshadow=compatible-local" }
      }
  }
};

S<int> s;
