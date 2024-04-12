// PR c++/107457
// { dg-do compile { target c++20 } }

template<class T>
using lambda = decltype([]() {});

template<class R, class F = lambda<R>>
void test() { }

int main() {
  test<int>();
}
