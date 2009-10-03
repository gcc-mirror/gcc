struct Foo {
  virtual void X();
  virtual void Y();
};
struct Bar: public Foo {
  Bar(Foo *);
  void Y();
};
void Baz() {
  Foo f;
  Bar b(&f);
}
