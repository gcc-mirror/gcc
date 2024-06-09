// PR c++/97202
// { dg-options "" }

template<typename T>
struct F
{
  F<T>();  // { dg-warning "template-id not allowed for constructor" "" { target c++20 } }
};

template<typename T>
inline F<T>::F() { }
