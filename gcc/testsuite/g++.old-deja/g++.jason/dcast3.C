// Testcase for tricky dynamic cast situations.

struct A {
  virtual void f () { }
};

struct B : public A { };
struct C : public B { };
struct D : public B { };
struct E : public C, public D { };

struct B2 : public virtual A { };
struct C2 : public B2 { };
struct D2 : public B2 { };
struct E2 : public C2, public D2 { };

int main ()
{
  E e;
  E2 e2;

  A* ap = (C*)&e;

  // ap points to base subobject of unique B; succeeds
  if (dynamic_cast <B*> (ap) == 0)
    return 1;

  ap = (C2*)&e2;
  // ap points to base subobject shared by two Bs; fails
  if (dynamic_cast <B2*> (ap) != 0)
    return 2;
}
