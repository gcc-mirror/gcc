// PR c++/64717
// { dg-do compile }
// { dg-options "-O2 -Wuninitialized -fsanitize=vptr" }

class Foo {};
Foo make_foo ();

struct Handle { virtual ~Handle (); };
Handle open (Foo);

void
bar ()
{
  Handle file (open (make_foo ())); // { dg-bogus "is used uninitialized" }
}
