// Build don't link:

template <class T> void foo(T);

template <class T> void bar(void (*)(T), T);

void baz() {
  bar<int>(foo, 1);
  bar(foo<int>, 1); // explicit args for foo don't help - XFAIL *-*-*
  bar<int>(foo<int>, 1); // not even here - XFAIL *-*-*
  bar(foo, 1); // ICE - XFAIL *-*-*
}
