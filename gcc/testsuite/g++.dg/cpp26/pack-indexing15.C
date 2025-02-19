// PR c++/113800
// { dg-do compile { target c++26 } }
// From LLVM's temp_arg_nontype_cxx2c.cpp.

template<class... T>
concept C = sizeof(T...[1]) == 1;

struct A {};

template<class T, C<T> auto = A{}> struct Set {};

template<class T>
void
foo ()
{
  Set<T> u;
}

Set<bool> sb;
Set<float> sf; // { dg-error "placeholder constraints not satisfied" }
