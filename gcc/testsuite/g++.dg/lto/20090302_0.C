/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-fPIC -flto -flto-partition=1to1 -r}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
struct Foo {
  bool Mumble();
  static void Bar() { if (foo_->Mumble()) foo_ = 0; }
  static void Baz() { Bar(); }
  static Foo *foo_;
};
void Unused() { Foo::Bar(); Foo::Baz(); }
