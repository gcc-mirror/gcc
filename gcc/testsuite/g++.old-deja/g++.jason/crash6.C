// Bug: g++ dies on the below.
// Build don't link:

class A { };
void f ()
{
  A a;
  a.~a();			// ERROR - causes segfault
}
