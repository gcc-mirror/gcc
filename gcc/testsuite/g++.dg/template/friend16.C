// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/9602: Inline friend/pure virtual tree data sharing in
// class template.

template <typename T> struct X {
  void foo (X);
  friend void bar () {}
};
    
template <typename T>
void X<T>::foo (X x) {}
    
template struct X<int>;
