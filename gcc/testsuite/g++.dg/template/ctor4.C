// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Jun 2004 <nathan@codesourcery.com>

// Origin Rani Sharoni via giovannibajo@libero.it
// Bug 16174, SFINAE failure.

template <class T> struct K 
{
  K();

  K(K<T> & rhs);
  K(K<T> const& rhs);
  template <class U> K(K<U> const& rhs);

private:
  template <class U> struct A;
  template <class U> struct A< K<U> const>
  {  typedef typename K<U>::compile_time_error type; };

  // This is used to reject calls to the copy constructor
  //  with objects which are top-level const. If they are
  //  const, the specialization of A is instantiated and
  //  causes a compile time error. Otherwise, the general
  //  template is picked up, it misses definition, so this
  //  ctor declaration is rejected by SFINAE and everybody
  //  is happy.
  // GCC 3.4.1pre and 3.5.0 always matches A's specialization
  //  when instantiating from foo(), and this causes the error.
  template <class U>
  K(U& rhs, typename A<U>::type = 0);
};


K<int> foo(void)
{
  return K<int>();
}
