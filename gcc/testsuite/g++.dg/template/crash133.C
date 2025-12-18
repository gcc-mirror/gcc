// PR c++/123186

template <class T>
struct A : T {
  typename A <T>::template B <42> C;
};
