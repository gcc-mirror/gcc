// PR c++/5453: Test that we don't assume that the pointer target of a
// reference is non-null just because we know the reference isn't.

// { dg-do run }

struct V { };
struct A: virtual public V { };

A* ap;
A*& apr (ap);

int main ()
{
  V* vp = apr;
}
