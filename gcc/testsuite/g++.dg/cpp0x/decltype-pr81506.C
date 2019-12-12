// PR c++/81506 - Invalid declaration with decltype accepted
// { dg-do compile }
//

#if __cplusplus < 201103L
# define decltype __typeof__
#endif

template <int>
struct A
{
  A () {
    decltype (this);     // { dg-error "declaration does not declare anything" }
  }
};

A<0> a;

template <class>
struct B
{
  B () {
    __typeof__ (this);   // { dg-error "declaration does not declare anything" }
  }
};

B<int> b;

