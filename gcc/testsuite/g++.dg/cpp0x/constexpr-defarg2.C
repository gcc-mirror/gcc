// PR c++/46368
// { dg-do compile { target c++11 } }

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
