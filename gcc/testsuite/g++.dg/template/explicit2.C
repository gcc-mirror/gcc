struct X {
  template <class B> void foo(B);
};

template <class D>
void bar() {
  X().foo<D>(1);
}

template void bar<int> ();
