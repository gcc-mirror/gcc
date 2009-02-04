// PR c++/39095
// { dg-do compile }

struct B
{
  int b;
};

struct A
{
  B *operator->();
  A ();
  B b;
};

A::A ()
{
}

B *
A::operator->()
{
  return &b;
}

A a;

int
foo ()
{
  return a->b;
}

// { dg-final { scan-assembler "_ZN1AptEv" } }
// { dg-final { scan-assembler-not "_ZN1AdtEv" } }
