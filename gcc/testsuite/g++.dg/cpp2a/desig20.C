// PR c++/101405
// { dg-do compile { target c++20 } }

struct A {
  int const a = 1;
  int const b = 2;
};

struct B : A {
  using A::a;
  using A::b;
  int const c = 3;
  int const d = 4;
};

int main()
{
  [[maybe_unused]] B b =
  { .a = 10, .d = 42 };		// { dg-error "not a direct member" }
}
