// { dg-do compile }

// Origin: Albert Chin <bugzilla-gcc@thewrittenword.com>
//	   Wolfgang Bangerth <bangerth@dealii.org>

// PR c++/14513, unqualified lookup of friend class.

struct S {
    void test (void);
};

namespace NS {
  class X {
      friend class S;
      static int *i;	// { dg-error "private" }
  };
}

void S::test () {
  NS::X::i;		// { dg-error "this context" }
}
