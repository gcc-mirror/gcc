// Build don't link:

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Mar 2001 <nathan@codesourcery.com>

// DR 209. friend access is not checked.

class Z;
class A
{
  class Inner {}; // ERROR - private
  Inner f (Inner);
  public:
  A ();
  friend class Z;
};

class B
{
  A::Inner j ();    // ERROR - private
  A::Inner k ();    // ERROR - private
  friend A::Inner A::f (Inner);
  int t;
  friend class A::Inner;
};

A::Inner l ();    // ERROR - private
A::Inner m ();    // ERROR - private
A::Inner a,       // ERROR - private
         b;       // ERROR - private
A::Inner a1;      // ERROR - private
int b2;

A::Inner A::f (Inner)
{
  B b;
  b.t = 1;
  return A::Inner ();
}

void Foo ()
{
  A::Inner i;  // ERROR - private
  class Local
  {
    friend A::Inner A::f (Inner);
    A::Inner k; // ERROR - private
  };
  class Local1 : A::Inner
  { // ERROR - private
    
  };
  A::Inner k;  // ERROR - private
}

class X : A::Inner
{  // ERROR - private
  class I : A::Inner
  {  // ERROR - private
    
  };
};

class Z : A::Inner
{  // ERROR - private
  class I : A::Inner
  { // ok, the base A::Inner is in the scope of Z which is a friend of A
    
  };
};
int b4;
