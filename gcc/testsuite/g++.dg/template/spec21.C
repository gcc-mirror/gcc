// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Mar 2005 <nathan@codesourcery.com>

// { dg-do run }
// DR214

template <class T> T f(int) {return 0;}
template <class T, class U> T f(U){return 1;}

template <typename T, typename R> T checked_cast (R const &) {return 0;}
template <typename T, typename R> T checked_cast (R *) {return 1;}


int main ()
{
  int i = 0;

  if (f<int>(1))
    return 1;
  
  if (checked_cast<int>(i) != 0)
    return 2;

  if (checked_cast<int>(&i) != 1)
    return 3;

  return 0;
}
