// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T> struct S { ~S(); };
int i;

void f () 
{
  i.~S(); // ERROR - invalid destructor call.
}
