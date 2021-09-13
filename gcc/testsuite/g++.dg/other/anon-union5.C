// PR c++/97974

struct A {
  union {
    operator int ();		// { dg-error "anonymous union" }
    int a;
  };
  operator int;			// { dg-error "" }
};
