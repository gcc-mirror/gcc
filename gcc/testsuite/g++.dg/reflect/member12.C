// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  int a;
  static int b;
  void foo () {}
  static void bar () {}
};
int S::b = 42;

void
baz ()
{
  S s {};
  s.[:^^S::a:]++;
  s.[:^^S::b:]++;
  s.[:^^S::foo:] ();
  s.[:^^S::bar:] ();
}

template <typename T>
void
qux ()
{
  S s {};
  s.[:(T) ^^S::a:]++;
  s.[:(T) ^^S::b:]++;
  s.[:(T) ^^S::foo:] ();
  s.[:(T) ^^S::bar:] ();
}

void
corge ()
{
  qux <decltype (^^int)> ();
}
