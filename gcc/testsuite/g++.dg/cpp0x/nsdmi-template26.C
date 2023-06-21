// PR c++/109506
// { dg-do link { target c++11 } }
// { dg-additional-options "-fchecking=2" }

template<class T>
struct foo {
  foo() { };
};

template<class T>
class bar {
  foo<int> alloc_{};
};

template<class T>
bar<int> func1() {
  return bar<int>{};
}

int main() {
  func1<int>();
}
