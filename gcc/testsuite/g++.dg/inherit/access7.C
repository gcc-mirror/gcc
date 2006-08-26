struct B {
  static void f();
};

template <typename T>
struct D : private B {
  void g() {
    f();
  }
};
  
void h() {
  D<int> d;
  d.g();
}
