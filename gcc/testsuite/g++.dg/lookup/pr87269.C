// { dg-do compile { target c++11 } }
// PR c++/87269 ICE failing to keep a lookup

namespace {
  void  operator"" _a (const char *, unsigned long) {}
}

void operator"" _a (unsigned long long);

template <typename> void f () { ""_a; }

void frob ()
{
  f<int> ();
}
