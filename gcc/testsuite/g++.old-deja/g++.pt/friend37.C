// { dg-do assemble  }
// Simplified from report by Volker Dobler <volker@hugo.physik.uni-konstanz.de>

template <class T> class A {
  friend int ice<>( int k=0 ); // { dg-error "" } default argument
  friend inline int f<>(double); // { dg-error "" } inline
};
