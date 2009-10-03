struct Foo {
  bool Mumble();
  static void Bar() { if (foo_->Mumble()) foo_ = 0; }
  static void Baz() { Bar(); }
  static Foo *foo_;
};
Foo *Foo::foo_;
