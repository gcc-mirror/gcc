// { dg-do assemble  }
// Bug: g++ dies on the below.

class A { };
void f ()
{
  A a;
  a.~a();			// { dg-error "" } causes segfault
}
