// { dg-do compile }

// Origin: Eelis van der Weegen <gccbugs@contacts.eelis.net>

// PR c++/10552: Member class template as template template argument
// substitution issue.

template <template <typename> class A, typename>
struct B
{
  typedef typename A<int>::t t;
};

template <typename D>
struct E
{
  template <typename> struct F { typedef int t; };
  typedef typename B<F, D>::t t;
};

typedef E<int>::t t;
