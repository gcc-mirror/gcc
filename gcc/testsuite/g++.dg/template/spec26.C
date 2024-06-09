// { dg-do compile { target c++11 } }
// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 16 Sep 2005 <nathan@codesourcery.com>

// PR 23519  template specialization ordering (DR214)
// Origin:  Maxim Yegorushkin <maxim.yegorushkin@gmail.com>

// DR532 clarified that the * expression is ambiguous.

struct A
{
  template<class T> int operator+(T&) = delete;
};

template<class T> struct B
{
  int operator-(A&) {return 2;}
  template<typename R> int operator*(R&) {return 3;}
};

template <typename T, typename R> int operator-(B<T>, R&) = delete;
template<class T> int operator+(A&, B<T>&) { return 5;}
template <typename T> int operator*(T &, A&){return 6;}

int main()
{
  A a;
  B<A> b;
  if ((a + b) != 5)
    return 1;
  
  if ((b - a) != 2)
    return 2;
  
  if ((b * a) != 6)		// { dg-error "ambiguous" }
    return 3;
}
