// PR c++/122628
// { dg-additional-options "-fmodules" }

template <typename T> static void baz(T) {}
template <typename T> static void bar(T) { baz(0); }

void foo() {
  bar(0);
}
