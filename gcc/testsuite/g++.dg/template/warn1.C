// { dg-do compile }
// { dg-options "-Wall" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Aug 2003 <nathan@codesourcery.com>

// PR 11512. erroneous warnings

template <class T>  void Foo(T i) 
{ 
  i++, i++;
  i, i++; // { dg-warning "left-hand operand" "" }
  i++, i; // { dg-warning "right-hand operand" "" }
  for (;; --i, ++i)
    ;
} 
 
void Bar ()
{ 
  Foo (1);  // { dg-error "instantiated" "" }
}

struct M {};

struct C
{
  M m;
  C () :m (M ()) {}
};


void Baz (int i)
{
  i ? i + 1 : i + 2; // { dg-error "operand of" "" }
  i ? i++ : 0;
}
