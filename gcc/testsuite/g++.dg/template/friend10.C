// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Dec 2002 <nathan@codesourcery.com>

// PR 5116. template instantiation can add a friend into a namespace,
// and thus change overload resolution.

#include <iostream>

static int right;
static int wrong;

struct Buggy {};

template <typename T>struct Handle
{
  Handle(T* p) {}
  
  operator bool() const { wrong++; return true; }
  
  friend std::ostream& operator<<(std::ostream& ostr, const Handle& r)
  {
    right++;
    
    return ostr << "in operator<<(ostream&, const Handle&)";
  }
};

typedef Handle<Buggy>     Buggy_h;

bool cmp (const Buggy_h& b1, const Buggy_h& b2)
{
  std::cout << b1 << " " << b2 << std::endl;
  return false;
}

int main()
{
  Buggy o;
  
  cmp (&o, &o);

  return !(right == 2 && !wrong);
}
