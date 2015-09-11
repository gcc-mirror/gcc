// PR c++/66392

struct B { };

struct A {
  explicit A (A const&);
  A (B const&);
};

int main () {
  A x = B ();
}
