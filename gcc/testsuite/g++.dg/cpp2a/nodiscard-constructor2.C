// PR c++/99362
// { dg-do compile { target c++20 } }

struct S {
  [[nodiscard]] S() {}
  S(int) {}
};

int main()
{
  S s;
  S();		// { dg-warning "ignoring return value" }
  (void)(S());
  S t = 1;
  S(1);
  (void)(S(1));
}
