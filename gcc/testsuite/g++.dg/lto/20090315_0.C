// { dg-lto-do run }
struct Foo {
  bool Mumble() { return true; }
  static void Bar() { if (foo_->Mumble()) foo_ = 0; }
  static void Baz() { Bar(); }
  static Foo *foo_;
};
Foo *Foo::foo_;
int main() { return 0; }
