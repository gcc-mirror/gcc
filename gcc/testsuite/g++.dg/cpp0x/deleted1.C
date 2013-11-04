// PR c++/58581
// { dg-do compile { target c++11 } }

template<typename T> int foo(T) noexcept(T()) = delete;

int i = foo(0);       // { dg-error "deleted" }
