// PR c++/46103
// { dg-options -std=c++11 }

struct MoveOnly {
  MoveOnly(const MoveOnly&) = delete;
  MoveOnly(MoveOnly&&) { }
  MoveOnly() = default;
};

struct A {
  MoveOnly mo[1];
  A() = default;
  A(A&&) = default;
};

int main() {
  A a;
  A aa = static_cast<A&&>(a);
}
