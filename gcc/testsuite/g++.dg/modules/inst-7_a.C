// PR c++/122625
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;
struct integral_constant {
  void f() const {}
};
struct span {
  template <int> static constexpr integral_constant __v{};
  template <int, int> static constexpr integral_constant partial{};
};
template <int x> constexpr integral_constant span::partial<x, 0>{};

template <typename T>
struct nested {
  template <typename U> static const U arr[3];
};
template <typename T>
struct nested<T*> {
  template <typename U> static const U arr[3];
};
template <typename T> template <typename U> const U nested<T>::arr[3] = {};
template <typename T> template <typename U> const U nested<T*>::arr[3] = {};
template <typename T> template <typename U> const U nested<T*>::arr<U*>[3] = {};

export inline void format() {
  span::__v<1>.f();
  span::partial<5, 0>.f();
  nested<int>::arr<integral_constant>[0].f();
  nested<int*>::arr<integral_constant>[0].f();
  nested<int*>::arr<integral_constant*>[0].f();
}
