// PR c++/23056
// { dg-do compile }

template <bool T> struct S { virtual ~S(); };
void foo ()
{
  static_cast<bool>("Foo");
}
S<true> a;
