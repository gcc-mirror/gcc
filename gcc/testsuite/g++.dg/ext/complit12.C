// PR c++/40948
// { dg-do run }
// { dg-options "" }

int c;
struct M
{
  M () { ++c; }
  M (const M&) { ++c; }
  ~M () { --c; }
};

struct S
{
  S ();
  M m[1];
};

S::S () : m ((M[1]) { M () })
{
}

struct T
{
  T ();
  M m[4];
};

T::T () : m ((M[4]) { M (), M (), M (), M () })
{
}

typedef M MA[1];
MA &bar (MA, MA& r) { return r; }

M f(M m) { return m; }

int main ()
{
  {
    M m[1] = (M[1]) { M () };
    if (c != 1)
      return 1;
    M n = (M) { M () };
    if (c != 2)
      return 2;
    M o[4] = (M[4]) { M (), M (), M (), M () };
    if (c != 6)
      return 3;
    S s;
    if (c != 7)
      return 4;
    T t;
    if (c != 11)
      return 5;
    MA ma = bar ((M[2]) { M(), M() }, m);
    if (c != 12)
      return 7;
    M mm[2] = ((M[2]) { f(M()), f(M()) });
    if (c != 14)
      return 8;
  }
  if (c != 0)
    return 6;
}
