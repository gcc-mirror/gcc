// { dg-do run }

// Copyright 2002 Free Software Foundation
// Contributed by Jason Merrill <jason@redhat.com>

// Make sure the GNU extension of accepting dropping cv-qualifiers for
// the implicit this argument does not kick in when taking the address
// of an object, since this extension would change the meaning of a
// well-defined program.

struct A {
  A* operator&() { return 0; }
};

int main ()
{
  const A a = {};
  return (&a == 0);
}
