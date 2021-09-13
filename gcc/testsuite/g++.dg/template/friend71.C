// PR c++/41723

template<class T>
class C {
  template <class U> class D {};

  friend class C::D<int>;
};
