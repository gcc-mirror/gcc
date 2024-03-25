// PR c++/112619
// { dg-do compile }

struct S { S (); ~S (); };

S
foo (int a, int b)
{
  if (a || b)
    {
      S s;
      return s;
    }
  return S ();
}
