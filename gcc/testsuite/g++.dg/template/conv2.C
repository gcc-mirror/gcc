// { dg-do run }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Dec 2001 <nathan@codesourcery.com>

// PR 4361. Template conversion operators were not overloaded.

class C
{
public:

  operator float () {return 2;}
  
  operator int () 
  {
    return 0;
  }
  
  template<typename T>
  operator int ()
  { return 1;
  }
};

int main ()
{
  C p;
  int r;

  r = p.operator int ();
  if (r)
    return r;
  r = static_cast <int> (p);

  if (r)
    return r + 2;
  
  return 0;
}
