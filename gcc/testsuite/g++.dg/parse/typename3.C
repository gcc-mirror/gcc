template <typename T>
struct D2 : public T::B {
  typedef typename T::X::Y Y;

  void f () {
    Y::f ();
  }
};
