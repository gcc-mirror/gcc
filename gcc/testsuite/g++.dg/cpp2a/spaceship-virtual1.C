// PR c++/95567
// { dg-do run { target c++20 } }

struct B {
  B(int i) : i(i) {}
  virtual ~B() = default;

  bool operator==(B const&) const = default;
  int i;
};

struct D : B {
  D(int i, int j) : B(i), j(j) {}
  int j;
};

int main() {
  if (B(2) != D(2, 3))
    __builtin_abort();
}
