// PR c++/57319
// { dg-require-effective-target c++11 }

namespace N1 {
  struct A { };
  struct B: virtual A { };
  struct C: virtual B { };

  struct D: C
  {
    void operator= (D &);
  };
}

namespace N2 {
  struct A { A& operator=(A&&); };
  struct B: virtual A { };	// { dg-warning "move assignment" }
  struct C: virtual B { };	// { dg-warning "move assignment" }

  struct D: C
  {
    void operator= (D &);
  };
}
