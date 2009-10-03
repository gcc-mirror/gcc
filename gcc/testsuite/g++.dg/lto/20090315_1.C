struct Foo {
  bool Mumble() { return true; }
  static void Bar() { if (foo_->Mumble()) foo_ = 0; }
  static void Baz() { Bar(); }
  static Foo *foo_;
};
void Unused() { Foo::Bar(); Foo::Baz(); }
