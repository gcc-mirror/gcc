// Build don't link:
// Simplified from report by Volker Dobler <volker@hugo.physik.uni-konstanz.de>

// crash test - XFAIL *-*-*

template <class T> class A {
  friend int ice<>( int k=0 ); // ERROR - undeclared
};
