// PR c++/100362
// { dg-do compile { target c++14 } }

template <class>
struct Qux {
  struct A { } a_;
  A f();

  void AsyncOp() {
    [](auto) {
      struct local : decltype(a_) {};
      local ptr;
    }(0);

    [](auto) {
      struct local : decltype(f()) {};
      local ptr;
    }(0);
  }
};

void corge() {
  Qux<int> qux;
  qux.AsyncOp();
}
