// { dg-do compile }
// Contributed by Matt Austern <austern at apple dot com>
// PR c++/13407: Gracefully handle keyword typename in base class specifier.

struct A { };
struct B { typedef A Type; };

template <typename T>
struct X : 
  public typename T::Type  // { dg-error "not allowed in this context" }
{ };

X<B> x;

struct C : 
  public typename A        // { dg-error "not allowed outside of templates" }
{ };
