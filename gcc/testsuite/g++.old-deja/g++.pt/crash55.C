// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

template<class T> struct foo {
  foo();
  void c();
  static void d(foo* x) { x->c(); }
};
template<class T> struct bar {
  bar();
};
template <class T> struct baz {
  typedef foo<T> t;
  t *e;
  baz();
  ~baz() { t::d(e); }
};
template <class T> void foo<T>::c()
{
  bar<T>* x = (bar<T>*)this;
  x->bar<T>::~bar();
}
void a(void)
{
  baz<char> b;
}
