// P1774R8 - Portable assumptions
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

int
foo (int x)
{
  [[assume (x == 42)]];
  return x;
}

int
bar (int x)
{
  [[assume (++x == 43)]];
  return x;
}

int
baz (int x)
{
  [[assume (({ int z = ++x; static int w; ++w; if (z == 51) return -1; if (z == 53) goto lab1; if (z == 64) throw 1; z == 43; }))]];
lab1:
  return x;
}

struct S { S (); S (const S &); ~S (); int a, b; int foo (); };

int
qux ()
{
  S s;
  [[assume (s.a == 42 && s.b == 43)]];
  return s.a + s.b;
}

int
S::foo ()
{
  [[assume (a == 42 && b == 43)]];
  return a + b;
}

int
corge (int x)
{
  [[assume (({ [[assume (x < 42)]]; x > -42; }))]];
  return x < 42;
}

int
garply (int x)
{
  [[assume (({ [[assume (++x < 43)]]; x > -42; }))]];
  return x < 42;
}
