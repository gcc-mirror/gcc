// Test that synthesizing the C copy constructor doesn't require B<int> to
// be complete.

template <class T>
struct B
{
  typename T::NT nt;
};

struct A
{
  A ();
  A (const A&);
  A (const B<int>&);
};

struct C: A { };

C c;
C c2(c);

