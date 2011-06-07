// PR c++/35075

template<int&> struct A {};

template<typename T> struct B
{
  static const T t;
  A<t> a;			// { dg-error "reference variable" }
};

B<int&> b;			// { dg-message "required" }
