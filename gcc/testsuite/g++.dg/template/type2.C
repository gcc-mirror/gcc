// { dg-do compile }
// Origin: Juan Carlos Arevalo-Baeza <jcab@JCABs-Rumblings.com>

// PR c++/8442
// Type template parameter incorrectly treated as template template
// parameter.

template <typename T> struct A {};

template <typename T> struct B
{
  template <typename U> struct C {};
  template <typename U> A<C<U> > foo(U);
};

B<void> b;
