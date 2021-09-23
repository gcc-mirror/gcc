// PR c++/100344
// { dg-do compile { target c++11 } }

template <class> constexpr int find_index() { return 1; }

template <int, class T> void foo(T);

template <class T> void get(T v) {
  foo<find_index<T>()>(v);
}

int main() {
  get(0);
}
