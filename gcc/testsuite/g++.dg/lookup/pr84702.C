// PR c++/84702 failure to mark overload to keep
// { dg-do compile { target c++11 } }

void a ();

namespace {
  void a (int);
}

template<void (&b)() = a>
void c () {
  c ();
}
