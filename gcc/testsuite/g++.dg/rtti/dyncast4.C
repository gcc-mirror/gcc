// Test to make sure that we keep searching if we don't find the type we
// want at the expected address.

// { dg-do run }

struct A
{
  virtual void f() {};
};

struct B: A { };

struct C: A { };

struct D: B, C { };

int main()
{
  D d;
  A* ap = static_cast<B*>(&d);
  C* cp = dynamic_cast<C*>(ap);
  if (cp == 0)
    return 1;
  else
    return 0;
}
