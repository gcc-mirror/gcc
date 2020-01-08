// PR c++/47877
// { dg-options "-fvisibility-inlines-hidden" }
// { dg-require-visibility "" }
// { dg-final { scan-hidden "_ZN3Foo3barIS_EEvv" } }

struct __attribute__((visibility("default"))) Foo {
  template <class C> inline void bar() {};
};

int main()
{
  Foo().bar<Foo>();
}
