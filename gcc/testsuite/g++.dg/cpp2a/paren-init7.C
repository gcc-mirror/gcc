// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do run { target c++2a } }

int h;
struct i {
  i() {}
  explicit i(i &) {}
  template <typename j> i(j &) { h++; }
};

int main() {
  {
    i a[6];
    auto [b, c, d, e, f, g] = a;
  }
  i a[6];
  auto [b, c, d, e, f, g](a);
  if (h != 6)
    __builtin_abort();
}
