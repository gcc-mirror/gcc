// PR c++/64194
// { dg-do compile { target c++14 } }

template <typename T> void g(void (*)(T)) { }

template <typename> auto id(int) { }
template <typename> auto id(char) { return 0; }

int main() {
  g(id<int>);
}
