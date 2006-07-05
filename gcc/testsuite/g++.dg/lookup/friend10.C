// PR c++/18681
// Bug: The friend declaration in A failed to give C::D access to A::B
// as specified in DR 45.

class A
{
  struct B;
  friend class C;
};

class C
{
  struct D
  {
    void f();
  };
};

void C::D::f()
{
  A::B* p;
}
