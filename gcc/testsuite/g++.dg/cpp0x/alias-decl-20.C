// PR c++/53858
// { dg-do compile { target c++11 } }

template <typename T>  struct s0 { typedef  T  tdef0; };
template <typename T>  struct s1 { typedef  T  tdef1; };
template <typename T>  using us1 = typename s1<T>::tdef1;
template <typename  T, typename  TT = typename  us1<T>::tdef0>  struct s2 {};

int main () { return 0; }
