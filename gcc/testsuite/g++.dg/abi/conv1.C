// { dg-options "-fabi-version=1" }

template<class T1>
struct A {
  typedef typename T1::X X;
  operator X() const;
};

template <class T0, class T1 >
struct B {
  typedef typename T1::X X;
  operator X() const; // { dg-error "" }
};
