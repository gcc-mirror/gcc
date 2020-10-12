// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template <class T, class U>
concept bool Same = __is_same(T, U);

struct test {
  void func(Same<int>... ints) {}
};

void func(Same<int>... ints) {}

int main()
{
  test t;
  t.func(1, 2, 3);
  func(1, 2, 3);

  t.func(1, 2, ""); // { dg-error "no match" }
  func(1, 2, ""); // { dg-error "no match" }
}
