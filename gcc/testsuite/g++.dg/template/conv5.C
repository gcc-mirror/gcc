// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2002 <nathan@codesourcery.com>

// PR 764. Failed to find friend in overload resolution

template <class T>
struct S
{
  friend bool operator== (const S&, const S&) {
    return true;
  }
};

int main ()
{
   // S<int> s;
  
  const S<int> *p = 0;
  *p == *p; // error
}
