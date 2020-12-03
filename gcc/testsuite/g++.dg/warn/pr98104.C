// PR c++/98104

#include <new>

struct B
{
  B ();
  int *a;
  char b;
};

struct D : public B {};
void bar (B *);

void
foo ()
{
  D d;
  bar (::new (static_cast<B*>(&d)) B);	// { dg-bogus "placement new constructing an object of type 'B' and size '\[0-9]*' in a region of type 'B' and size '\[0-9]*'" }
}
