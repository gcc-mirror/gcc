// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -w

int result;

struct A {
  A ();

  int i;
};

A* ap;

A::A ()
{
  ap = this;
}

struct B : virtual public A
{
  B ();
  ~B ();

  int j;
};

B::B () {
  if ((A*) this != ap)
    result = 1;
}

B::~B () {
  if ((A*) this != ap)
    result = 1;
}

struct C : public B {
};

struct D : public C, public B
{
};

int main ()
{
  {
    D d;
  }

  return result;
}
