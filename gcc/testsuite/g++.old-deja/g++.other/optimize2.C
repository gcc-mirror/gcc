// Special g++ Options: -O2
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 May 2001 <nathan@codesourcery.com>

// Bug 2823. Inlineing the body of a thunk broke things. But that's
// rarely a sensible thing to do anyway.

#include <cstdio>
#include <cstdlib>

int objCount = 0;

struct Thing
{
  int count;

  Thing ();
  Thing (Thing const &src);
  
  ~Thing ();
  
};

Thing::Thing ()
  :count (0)
{
  objCount++;
  std::printf ("%p %s\n", (void *)this,__PRETTY_FUNCTION__);
}

Thing::Thing (Thing const &src)
  :count (0)
{
  objCount++;
  std::printf ("%p %s\n", (void *)this, __PRETTY_FUNCTION__);
}

Thing::~Thing ()
{
  std::printf ("%p %s\n", (void *)this, __PRETTY_FUNCTION__);
  if (count)
    std::abort ();
  count--;
  objCount--;
}

void x(Thing name)
{
  // destruct name here
}

class Base
{
  public:
  virtual void test(const Thing& s) = 0;
};

class Impl : virtual public Base
{
  public:
  virtual void test(const Thing& s)
  {
    x(s); // copy construct temporary
  }
};

int main()
{
  Impl *impl = new Impl();
  
  impl->test( Thing ());	// This will use a thunk
  return objCount != 0;
}
