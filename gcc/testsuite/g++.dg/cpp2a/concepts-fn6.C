// PR c++/96647
// { dg-do compile { target c++20 } }

template<typename T>
struct Base {
  auto f(int) { }
  auto f(int) requires T::fail { static_assert(T::fail); }
};

void (Base<void>::*ptr)(int) = &Base<void>::f;
