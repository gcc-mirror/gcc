// Build don't link:
// Simplified from report by Volker Dobler <volker@hugo.physik.uni-konstanz.de>

template <class T> class A {
  friend int ice<>( int k=0 ); // ERROR - default argument
  friend inline int f<>(double); // ERROR - inline
};
