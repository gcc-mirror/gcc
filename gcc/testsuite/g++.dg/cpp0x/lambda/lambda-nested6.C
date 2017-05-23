// PR c++/80267
// { dg-do compile { target c++11 } }

template <typename> void a() {
  int b;
  auto &c = b;
  [&] {
    c;
    [&] { c; };
  };
}
void d() { a<int>(); }
