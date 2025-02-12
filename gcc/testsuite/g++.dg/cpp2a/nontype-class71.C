// PR c++/113800
// { dg-do compile { target c++20 } }
// From LLVM's temp_arg_nontype_cxx2c.cpp.

template<class T, int I>
struct A {
  T x[I];
};

template<class T, class... U>
A(T, U...) -> A<T, 1 + sizeof...(U)>;

template<A a> void foo() { }

void
bar ()
{
  foo<{1}>();
}
