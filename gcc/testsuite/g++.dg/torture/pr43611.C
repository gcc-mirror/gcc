// { dg-do compile }
// { dg-options "-fkeep-inline-functions" }

template < typename >
struct A {
  void init (int);
  A ()
  {
    this->init (0);
  }
};

template < typename >
struct B : A < int > {
  A < int > a;
  B () {}
};

extern template struct A < int >;
extern template struct B < int >;

B < int > b;
