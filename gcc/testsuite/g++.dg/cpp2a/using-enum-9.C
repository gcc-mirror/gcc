// PR c++/100862
// { dg-do compile { target c++20 } }

enum class fruit { orange, apple };

struct A {
public:
  using enum fruit;
private:
};

struct B {
protected:
  using enum fruit;
public:
};

struct C {
private:
  using enum fruit;
public:
};

int main() {
  A::orange, A::apple;
  B::orange, B::apple; // { dg-error "protected" }
  C::orange, C::apple; // { dg-error "private" }
}
