// Build don't link:

template <int I>
void f()
{
  class C { public: int c; };

  struct S {
    void g() {
      C e;
      e.c = 3;
    };
  };

  S s;
  s.g();
}

template void f<7>();
