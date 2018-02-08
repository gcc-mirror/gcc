// { dg-do compile }
// { dg-options "-Wattributes" }

#define ATTR(list) __attribute__ (list)

template <int>
struct A
{
  int __attribute__ ((noinline))
  f ();                       // { dg-message "previous declaration here" }
};

template <int N>
int __attribute__ ((always_inline))
A<N>::f ()                    // { dg-warning "ignoring attribute .always_inline. because it conflicts with attribute .noinline." } */
{ return 0; }


template <int>
struct B
{
  int __attribute__ ((always_inline))
  f ();
};

template <>
inline int __attribute__ ((always_inline))
B<0>::f ()
{ return 0; }

template <>
int __attribute__ ((noinline))
B<1>::f ()
{ return 1; }
