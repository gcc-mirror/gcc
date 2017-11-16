// Test the self-assignemnt detection plugin with the 'disable' argument.
// { dg-do compile }
// { dg-options "-O -fplugin-arg-selfassign-disable" }

class Foo {
 private:
  int a_;

 public:
  Foo() : a_(a_) {} // { dg-bogus "assigned to itself" }

  void setA(int a) {
    a_ = a_; // { dg-bogus "assigned to itself" }
  }

  void operator=(Foo& rhs) {
    this->a_ = rhs.a_;
  }
};

struct Bar {
  int b_;
  int c_;
};

int g = g; // { dg-bogus "assigned to itself" }
Foo foo = foo; // { dg-bogus "assigned to itself" }

void func()
{
  Bar *bar1, bar2;
  Foo local_foo;
  int x = x; // { dg-bogus "assigned to itself" }
  static int y = y; // { dg-bogus "assigned to itself" }
  float *f;
  Bar bar_array[5];
  char n;
  int overflow;

  *f = *f; // { dg-bogus "assigned to itself" }
  bar1->b_ = bar1->b_; // { dg-bogus "assigned to itself" }
  bar2.c_ = bar2.c_; // { dg-bogus "assigned to itself" }
  local_foo = local_foo; // { dg-bogus "assigned to itself" }
  foo = foo; // { dg-bogus "assigned to itself" }
  foo.setA(5);
  bar_array[3].c_ = bar_array[3].c_; // { dg-bogus "assigned to itself" }
  bar_array[x+g].b_ = bar_array[x+g].b_; // { dg-bogus "self-assignment detected" }
  y = x;
  x = y;
}
