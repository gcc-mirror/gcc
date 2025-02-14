// PR c++/118856
// { dg-do run { target c++11 } }
// { dg-additional-options -O2 }

int a, b, c, d;
struct A { A () { ++a; ++d; }; ~A () { --a; ++d; }; };
struct B { B (const A & = A {}) { ++b; ++d; }; ~B () { --b; ++d; }; };
struct C {
  C (const B &, const A & = A {}) { ++c; ++d; throw 42; };
  ~C () { --c; ++d; };
  int *begin () { return nullptr; };
  int *end () { return nullptr; };
};

void
foo ()
{
  for (auto &i : C { B {} })
    ;
}

int
main ()
{
  try
    {
      foo ();
      __builtin_abort ();
    }
  catch (int x)
    {
      if (x != 42 || a || b || c != 1 || d != 7)
        __builtin_abort ();
    }
}
