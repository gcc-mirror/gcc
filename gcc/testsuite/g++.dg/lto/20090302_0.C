/* { dg-lto-do link } */
/* { dg-lto-options {{-fPIC -fwhopr -shared}} } */
struct Foo {
  bool Mumble();
  static void Bar() { if (foo_->Mumble()) foo_ = 0; }
  static void Baz() { Bar(); }
  static Foo *foo_;
};
void Unused() { Foo::Bar(); Foo::Baz(); }
