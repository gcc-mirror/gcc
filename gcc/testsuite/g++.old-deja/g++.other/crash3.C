// { dg-do assemble  }
// { dg-options "-g -O2" }

inline void f() {
  struct S {};
  S s;
}

void g()
{
  for (int i = 0; i < 2; ++i)
    f();
}

