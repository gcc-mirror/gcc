// PR lto/53808
// Devirtualization should not produce an external ref to ~bar.
// { dg-options "-O2" }
// { dg-final { scan-assembler-not "_ZN3barD0Ev" } }

struct foo {
 virtual ~foo();
};
struct bar : public foo {
 virtual void zed();
};
void f() {
 foo *x(new bar);
 delete x;
}
