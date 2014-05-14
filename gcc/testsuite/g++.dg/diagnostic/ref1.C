// PR c++/20332

struct bar {};
void foo1() {
  bar& b = bar();		// { dg-error "rvalue" }
}
void foo(bar& b = bar()) {}	// { dg-error "rvalue" }
