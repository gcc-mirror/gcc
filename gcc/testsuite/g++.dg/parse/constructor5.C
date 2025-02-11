// PR c++/118304
// { dg-do "compile" { target c++11 } }

// Constructors.
struct A {
  *A () = default;	      // { dg-error "return type specification" }
};
struct B {
  int* B () = default;	      // { dg-error "return type specification" }
};
struct C {
  const int& C () = default;  // { dg-error "return type specification" }
};
struct D {
  **D () = default;	      // { dg-error "return type specification" }
};
struct E {
  &E () = default;	      // { dg-error "return type specification|reference to" }
};
struct F {
  *&F () = default;	      // { dg-error "return type specification|reference to" }
};
struct G {
  **&G () = default;	      // { dg-error "return type specification|reference to" }
};
struct H {
  *H (const H&) = default;    // { dg-error "return type specification" }
};
struct I {
  **I (const I&) = default;    // { dg-error "return type specification" }
};
struct J {
  &J (const J&) = default;	// { dg-error "return type specification|reference to" }
};
struct K {
  const K() = default;		// { dg-error "expected unqualified-id" }
};

// Destructors.
struct L {
  * ~L () = default;		// { dg-error "return type specification" }
};
struct M {
  ** ~M () = default;		// { dg-error "return type specification" }
};
struct N {
  & ~N () = default;		// { dg-error "return type specification|reference to" }
};
