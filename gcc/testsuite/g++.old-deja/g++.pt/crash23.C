// { dg-do assemble  }

template <class A, class B> void foo();
template <class C> class bar {
public:
  int i;
  template <class B> friend void foo<C,B>(); // { dg-error "34:invalid use of template-id" }
};
template <class A, class B> void foo() {
  bar<A> baz; baz.i = 1;
  bar<int> buz; buz.i = 1;
}
int main() {
  foo<void,void>();
  foo<int,void>();
}
