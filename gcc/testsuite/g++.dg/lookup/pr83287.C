// PR c++/83287 failed to keep lookup until instantiation time

void foo ();

namespace {
  void foo (int);
}

template <class T>
void bar ()
{
  T (*p)() = (T (*)(void)) foo;
}

void
baz ()
{
  bar<void> ();
}
