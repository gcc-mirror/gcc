// { dg-do assemble  }

template <class T> void foo(T);

template <class T> void bar(void (*)(T), T);

void baz() {
  bar<int>(foo, 1);
  bar(foo<int>, 1);
  bar<int>(foo<int>, 1);
  bar(foo, 1); 
}
