// Build don't link:
// Simplified from report by Volker Dobler <volker@hugo.physik.uni-konstanz.de>

template <class T> class A {
  friend int ice<>( int k=0 ); // gets bogus error - ICE - XFAIL *-*-*
};
