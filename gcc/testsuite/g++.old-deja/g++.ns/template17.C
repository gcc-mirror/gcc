// { dg-do assemble  }
// 
// Copyright (C) 2001, 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Apr 2001 <nathan@codesourcery.com>

// Bug 2258. We failed to implement using directives inside template
// functions. This makes us regress now that ::std is real.

namespace thing
{
  template <typename T> T end2 (T);
}
namespace whatever 
{
}

template <typename T> void fn (T, T (*)(T));

namespace whatever
{
  template <typename T> T end3 (T);
}

template <class T> void mycout(const T& data)
{
  using namespace thing;
  using namespace whatever;
  
  fn (data, end2);
  fn (data, end3);
}

int main()
{
  double data = 5.0;
  mycout(data);
}
