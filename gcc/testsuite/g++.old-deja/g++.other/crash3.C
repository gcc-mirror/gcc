// Build don't link:
// Special g++ Options: -g -O2

inline void f() {
  struct S {};
  S s;
}

void g()
{
  for (int i = 0; i < 2; ++i)
    f();
}

