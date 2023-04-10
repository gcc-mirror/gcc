// PR c++/108606
// { dg-do compile { target c++14 } }

template <typename T>
void bar (T) {}

void
foo ()
{
  bar ([&] (auto x) { class C { friend void baz (); }; });
}
