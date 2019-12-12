// { dg-do compile { target c++17 } }

template<class, int>
struct Array {};

template<class T, int size_>
struct Foo {
  static constexpr int size() {
      return size_;
  }

  template<class U>
  Foo(U, Array<T, size()>) {}
};

template<class T, int size, class U>
Foo(U, Array<T, size>) -> Foo<T, size>;

int main() {
  Array<int, 2> arr{};

  Foo foo{2.0, arr};
}
