/* { dg-lto-do link } */
/* { dg-lto-options {{-ftoplevel-reorder -flto -flto-partition=none} {-ftoplevel-reorder -flto -flto-partition=1to1}} } */
/* { dg-extra-ld-options {-r -nostdlib -flinker-output=nolto-rel} } */

struct Foo { virtual ~Foo(); };
struct Bar:public Foo { Bar() { } };
void Func() { new Bar(); }
