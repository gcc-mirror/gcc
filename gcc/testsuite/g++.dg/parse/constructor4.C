// PR c++/118306
// { dg-do "compile" }

// Constructors.
struct A {
  *A ();	    // { dg-error "return type specification" }
};
struct B {
  **B ();	    // { dg-error "return type specification" }
};
struct C {
  ***C ();	    // { dg-error "return type specification" }
};
struct D {
  &D ();	    // { dg-error "return type specification|reference to" }
};
struct E {
  *&E ();	    // { dg-error "return type specification|reference to" }
};
struct F {
  **&F ();	    // { dg-error "return type specification|reference to" }
};
struct G {
  *G (const G&);    // { dg-error "return type specification" }
};
struct H {
  **H (const H&);    // { dg-error "return type specification" }
};
struct I {
  &I (const I&);    // { dg-error "return type specification|reference to" }
};
struct J {
  const J();	    // { dg-error "expected unqualified-id" }
};

// Destructors.
struct K {
  * ~K ();	    // { dg-error "return type specification" }
};
struct L {
  ** ~L ();	    // { dg-error "return type specification" }
};
struct M {
  & ~M ();	    // { dg-error "return type specification|reference to" }
};
struct N {
  virtual * ~N ();  // { dg-error "return type specification" }
};
struct O {
  virtual & ~O ();  // { dg-error "return type specification|reference to" }
};
struct P {
  volatile ~P();    // { dg-error "qualifiers are not allowed" }
};
