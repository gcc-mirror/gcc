// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Sep 2003 <nathan@codesourcery.com>
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// PR c++/11788 we failed to instantiate a decl, and we lost some side
// effects

static int flag = 0;

template <typename> struct A
{
  A &active ()  { flag++;}
  
  static void foo() {}

  static void bar () {}
  static void bar (int) {}

  int m;
};

void (*baz ()) ()
{
    A<int> a;
    return &a.active ().foo;
}

void (*boz ()) ()
{
    A<int> a;
    return &a.active ().bar;
}

int *buz ()
{
  A<int> a;
  
  return &a.active ().m;
}

int main ()
{
  baz ();
  boz ();
  buz ();
  
  return flag != 3;
}
