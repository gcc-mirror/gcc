// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 16 Jul 2003 <nathan@codesourcery.com>

// A non-dependent field_decl can bind at parse time.

template <class T>
struct Foo {
  int j; // we never see this one.
  int k; // { dg-error "" "" }
  
};

struct Baz 
{
  int j;
  int k; // { dg-error "" "" }
  
};

template <class T>
struct Bar : public Foo<T>, Baz {
  
  int baz () { return j; } // binds to Baz::j
  int foo () { return this->k; } // { dg-error "request for member" "" }
};

int main()
{
  Bar<int> bar;

  bar.baz ();
  bar.foo (); // { dg-error "instantiated" "" }
  
  return 0;
}
