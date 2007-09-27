// Don't crash on an unknown attribute.

struct foo {
  template <class T>
  void __attribute__((leafify)) bar() {} // { dg-warning "ignored" }
};

void bar(void)
{
  foo f;
  f.bar<int>();
}
