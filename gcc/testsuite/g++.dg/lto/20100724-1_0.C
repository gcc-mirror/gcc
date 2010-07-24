/* { dg-lto-do link } */
/* { dg-lto-options {{-ftoplevel-reorder -flto} {-ftoplevel-reorder -fwhopr}} } */
/* { dg-extra-ld-options {-r -nostdlib} } */

struct Foo { virtual ~Foo(); };
struct Bar:public Foo { Bar() { } };
void Func() { new Bar(); }
