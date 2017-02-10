// PR c++/79401
// { dg-do compile { target c++11 } }

class B
{
protected:
  B (int, int);
};
class C : public B
{
protected:
  using B::B;
};
class A : public C
{
  A (char *);
};
A::A (char *) : C (0, 0)
{
}
