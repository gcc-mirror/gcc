// { dg-lto-do assemble }
// { dg-lto-options {{-O2 -flto -fno-strict-aliasing}} }

namespace ns
{
  template <class> class hash_set { };
}

struct Foo
{
  long long f1, f2, f3;
};

void func(ns::hash_set<int>) {
  Foo foo = { 0, 0, 0 };
}
