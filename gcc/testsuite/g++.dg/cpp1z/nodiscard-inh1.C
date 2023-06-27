// [[nodiscard]] should apply to inherited constructors.
// { dg-do compile { target c++11 } }

struct A {
  [[nodiscard]] A(int);
};

struct B: A {
  using A::A;
};

int main()
{
  B(42);				// { dg-warning nodiscard }
}
