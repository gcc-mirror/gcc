// I hate this type of test case.  I'm not sure how to code it better.
// See the PR for what this tests.
// prms-id: 7325
// execution test - XFAIL *-*-*

int fail = 0;

struct A {
  int i;
  static const A* match_this;
  static const A* match_arg;
  A(): i(7) {
    if (match_this)
      if (match_this != this)
	fail = 1;
  }
  A* get_this() { return this; }
  A& operator = (const A& o) {
    if (match_this)
      if (match_this != this)
	fail = 1;
    if (match_arg)
      if (match_arg != &o)
	fail = 1;
    match_arg = &o;
    return *this;
  }
};

const A* A::match_this;
const A* A::match_arg;
A a;

A foo() { return a; }
void f ()
{
  A a;
  A::match_this = &a;
  a = foo ();
  a = foo ();
  A::match_this = 0;
}

void g ()
{
  A::match_this = A().get_this();
  A();
  A();
  A::match_this = 0;
}

int main() {
  f();
  g();
  return fail;
}
