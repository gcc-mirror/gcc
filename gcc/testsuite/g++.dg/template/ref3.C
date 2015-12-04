// PR c++/28341

template<const int&> struct A {};

template<typename T> struct B
{
  A<(T)0> b; // { dg-error "constant|not a valid" }
  A<T(0)> a; // { dg-error "constant|not a valid" "" { xfail c++98_only } }
                                                       // PR c++/68699
};

B<const int&> b;
