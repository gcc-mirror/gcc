// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sept 2000 <nathan@codesourcery.com>

// bug 73. We failed to compare explicit arguments for a TEMPLATE_ID_EXPR in a
// TYPENAME_TYPE.

struct Plus {
  template <class T>
  struct Sig { typedef int Third;};
};


template <class T>
struct Ethel {
  typedef int WrappedType;
};

struct Fred {
   
  template <class Q, class LA, class LB>
  Ethel<typename Q::template Sig<typename LA::WrappedType>::Third> baz ();


  template <class Z, class A, class B>
  Ethel<typename Z::template Sig<A>::Third>
  foo ( const Z&, const Ethel<A>&, const Ethel<B>&) const;
};

int main() {
  Fred f;
  Ethel<int> e;
  Plus p;
 
  f.foo (p, e, e);

  return 0;
}
