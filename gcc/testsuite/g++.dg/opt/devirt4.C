// PR lto/53808
// Devirtualization + inlining should produce a non-virtual
// call to ~foo.
// { dg-options "-O -fdevirtualize" }
// { dg-final { scan-assembler "_ZN3fooD2Ev" } }

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
