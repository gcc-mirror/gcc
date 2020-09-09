// PR c++/96647
// { dg-do compile { target c++11 } }

template<typename>
struct Base {
  auto f(int) { }
  auto f(char) { }
};

void (Base<void>::*ptr)(int) = &Base<void>::f;
