// PR c++/17585

template <void (*p)(void)> struct S03 {};
class C03 {
public:
  static void f(void) {}
  void g(void) { S03<&f> s03; }
};
