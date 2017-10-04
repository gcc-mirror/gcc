// { dg-do run }
// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool Class () { return __is_class(T); }

struct Test {
  void f(auto a) requires Class<decltype(a)>();
} test;

struct S { }s;

int main() {
  test.f(s);
}

void Test::f(auto a) requires Class<decltype(a)>() { }
