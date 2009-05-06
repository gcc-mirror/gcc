// Test case for the dumb plugin.
// { dg-do compile }
// { dg-options "-O -fplugin-arg-dumb_plugin-ref-pass-name=ccp -fplugin-arg-dumb_plugin-ref-pass-instance-num=1" }

class Foo {
 private:
  int a_;

 public:
  Foo() : a_(a_) {} // { dg-warning "Before genericizing function" }

  void setA(int a) {
    a_ = a_;
  } // { dg-warning "Before genericizing function" }

  void operator=(Foo& rhs) {
    this->a_ = rhs.a_;
  } // { dg-warning "Before genericizing function" }
}; // { dg-warning "Process struct Foo" }

struct Bar {
  int b_;
  int c_;
}; // { dg-warning "Process struct Bar" }

int g = g;
Foo foo = foo;

int func()
{
  Bar *bar1, bar2;
  Foo local_foo;
  int x = x;
  static int y = y;
  float *f;
  Bar bar_array[5];
  char n;
  int overflow;

  *f = *f;
  bar1->b_ = bar1->b_;
  bar2.c_ = bar2.c_;
  local_foo = local_foo;
  foo = foo;
  foo.setA(5);
  bar_array[3].c_ = bar_array[3].c_;
  bar_array[x+g].b_ = bar_array[x+g].b_;
  y = x;
  x = y;
} // { dg-warning "Before genericizing function" }

// { dg-warning "Analyze function" "" { target *-*-* } 50 }
// { dg-warning "End of compilation unit" "" { target *-*-* } 50 }
