// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Mar 2002 <nathan@codesourcery.com>

// PR c++/5659. Failed to notice default accessed changed

class Foo;
struct Foo 
{
  static int m;
};

class Outer {
  private:
  class Inner;
  Inner *i;
  public:
  void pub();
};

struct Outer::Inner {
  Inner(int i);
};

void Outer::pub() { i = new Inner(Foo::m); }
