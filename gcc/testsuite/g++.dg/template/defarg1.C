struct Outer {
    template <int I, int J=I> struct Inner {};
};

void f() {
  Outer::Inner<2> i;
}
