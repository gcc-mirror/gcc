// PR middle-end/22484
// { dg-do compile }
// { dg-options "-O3" }

struct A { ~A(); };
typedef bool B;

bool foo();

bool bar(A&)
{
  B b = true;

  for (int i = 0; i < 2 && b; ++i)
    b = foo();

  return b;
}

void baz()
{
  A a;
  if (bar(a)) foo();
}
