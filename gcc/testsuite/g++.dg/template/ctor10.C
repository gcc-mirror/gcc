// PR bootstrap/105567
// This was breaking with cdtor_returns_this.

template <class T>
struct A
{
  A() { return; }
};

A<int> a;
