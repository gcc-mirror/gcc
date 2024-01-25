// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

int g = 42;

struct [[gnu::no_dangling]] A {
  ~A();
  int *i;
  int &foo() { return *i; }
};

struct A2 {
  ~A2();
  int *i;
  [[gnu::no_dangling]] int &foo() { return *i; }
  [[gnu::no_dangling]] static int &bar (const int &) { return *&g; }
};

union [[gnu::no_dangling]] U { };

A a() { return A{&g}; }
A2 a2() { return A2{&g}; }

class X { };
const X x1;
const X x2;

[[gnu::no_dangling]] const X& get(const int& i)
{
   return i == 0 ? x1 : x2;
}

void
test ()
{
  [[maybe_unused]] const X& x = get (10);	// { dg-bogus "dangling" }
  [[maybe_unused]] const int &i = a().foo();	// { dg-bogus "dangling" }
  [[maybe_unused]] const int &j = a2().foo();	// { dg-bogus "dangling" }
  [[maybe_unused]] const int &k = a2().bar(10);	// { dg-bogus "dangling" }
}
