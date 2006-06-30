// PR c++/26577
// The call to bar() was causing an inappropriate dereference of *this,
// which led to an abort in cp_expr_size.

struct A
{
  A(const A&);
  A& operator=(const A&);
  static void bar();
  void baz() volatile;
};

void A::baz() volatile
{
  bar();
}
