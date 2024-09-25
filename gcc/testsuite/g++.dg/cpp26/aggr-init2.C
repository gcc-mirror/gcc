// P3106R1 - Clarifying rules for brace elision in aggregate initialization
// Examples from C++26 [dcl.init.aggr]
// { dg-do compile }

namespace N1 {
#if __cpp_designated_initializers >= 201707L
  struct C {
    union {
      int a;
      const char* p;
    };
    int x;
  };
  constexpr C c2 = { .a = 42.0, .x = 3 };	// { dg-error "narrowing conversion of '4.2e\\\+1' from 'double' to 'int'" "" { target c++20 } }
#endif
}

namespace N6 {
#if __cplusplus >= 201103L
  struct S {
    int y[] = { 0 };	// { dg-error "ISO C\\\+\\\+ forbids flexible array member 'y'" "" { target c++11 } }
			// { dg-error "flexible array member 'N6::S::y' in an otherwise empty 'struct N6::S' is a GCC extension" "" { target c++11 } .-1 }
			// { dg-error "initializer for flexible array member 'int N6::S::y \\\[\\\]'" "" { target c++11 } .-2 }
  };
#endif
}

namespace N8 {
#if __cplusplus >= 201402L
  struct A;
  extern A a;
  struct A {
    const A &a1 { A { a, a } };
    const A &a2 { A { } };	// { dg-error "default member initializer for 'N8::A::a2' required before the end of its enclosing class" "" { target c++14 } }
  };				// { dg-error "invalid initialization of reference of type 'const N8::A\\\&' from expression of type '<brace-enclosed initializer list>'" "" { target c++14 } .-1 }
  A a { a, a };

  struct B {
    int n = B {}.n;		// { dg-error "default member initializer for 'N8::B::n' required before the end of its enclosing class" "" { target c++14 } }
  };

  struct C;
  extern C c;
  struct C {
    const C &c1 { C { c, c } };
    const C &c2 { C { c, c } };
  };
  C c { c, c };
#endif
}

namespace N11 {
  char cv[4] = { 'a', 's', 'd', 'f', 0 };	// { dg-error "too many initializers for 'char \\\[4\\\]'" }
}

namespace N15 {
  union u { int a; const char* b; };
  u a = { 1 };
  u b = a;
  u c = 1;                        // { dg-error "conversion from 'int' to non-scalar type 'N15::u' requested" }
  u d = { 0, "asdf" };            // { dg-error "too many initializers for 'N15::u'" }
  u e = { "asdf" };               // { dg-error "invalid conversion from 'const char\\\*' to 'int'" }
#if __cpp_designated_initializers >= 201707L
  u f = { .b = "asdf" };
  u g = { .a = 1, .b = "asdf" };  // { dg-error "too many initializers for 'N15::u'" "" { target c++20 } }
#endif
}
