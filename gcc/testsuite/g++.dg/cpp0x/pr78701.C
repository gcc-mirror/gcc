// PR c++/58707
// { dg-do compile { target c++11 } }

// ICE during deduction of default parms

template <class T, T N = T(), bool B = N>
  void f(T x) {}

template void f<int> (int);
