// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T> struct S { ~S(); };
int i;

void f () 
{
  i.~S(); // { dg-error "" } invalid destructor call.
}
