// PR c++/95451
// { dg-do run { target c++14 } }

extern "C" void abort ();

struct A {
  template <typename>
  void foo ()
  {
    auto b = [this] (auto) { return operator () (); } (0);
    if (b != 3)
      abort ();
    auto c = [this] (int) { return operator () (); } (0);
    if (c != 3)
      abort ();
  }
  void bar ()
  {
    auto d = [this] (auto) { return operator () (); } (0);
    if (d != 3)
      abort ();
    auto e = [this] (int) { return operator () (); } (0);
    if (e != 3)
      abort ();
  }
  int operator () () { return 3; }
};

int
main ()
{
  A a;
  a.foo<void> ();
  a.bar ();
}
