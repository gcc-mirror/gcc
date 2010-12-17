// PR c++/46368
// { dg-options "-std=c++0x" }

class A;

class B
{
  A foo ();
  A bar ();
};

class C
{
};

struct D
{
  D (C);
};

struct A : D
{
  A (const C & n) : D (n) {}
};

A baz (const char *, A = C ());

A
B::foo ()
{
  try
    {
      baz ("foo");
    }
  catch (...)
    {
    }
}

A
B::bar ()
{
  baz ("bar");
}
