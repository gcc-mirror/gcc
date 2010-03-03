// { dg-do assemble  }
// { dg-prune-output "mangled name" }
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 25 Jul 2001 <nathan@codesourcery.com>

// Origin: gustavo@geneura.ugr.es
// Bug 3624. Template instantiation of a reference type was not
// converted from reference when doing a call.

#include <iostream>

using namespace std;

template <class A, class B, class C, C& c, bool d> class eo: public A
{
public:
  eo() 
  {
    cout << this->x << " " << this->y << " "
	 << c(*this) << " "
	 << ((d)?"true":"false") << endl;
  }
  
private:
  B b;
};

struct XY
{
  float x, y;

  XY(): x(1), y(0.1) {}
};

float fitness(const XY& a)
{
  return a.x + a.y;
}

struct fitness2
{
  float operator()(const XY& a)
  {
    return a.x - a.y;
  }
  
  float f(const XY& a)
  {
    return a.x - a.y;
  }
};

struct fitness3
{
  float operator()(const XY& a)
  {
    return a.x / a.y;
  }
};

fitness2 f2;
fitness3 f3;

int main()
{
  eo<XY, float, fitness2, f2, true> eo2;
  eo<XY, float, fitness3, f3, true> eo3;

  return 0;
}
