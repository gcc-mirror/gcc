// PR c++/95317
// { dg-do compile { target c++14 } }

template <typename> void fn1() {
  [](auto) {
    enum { VALUE };
    VALUE;
  };
}
int main() { fn1<void>; }
