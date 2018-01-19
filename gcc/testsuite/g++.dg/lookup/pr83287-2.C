// PR c++/83287 failed to keep lookup until instantiation time

void foo ();

namespace {
  void foo ();
}

template <class T>
void
bar ()
{
  new T (foo); // { dg-error "cannot resolve" }
}

void
baz ()
{
  bar <double> ();
}
