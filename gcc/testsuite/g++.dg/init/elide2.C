// PR c++/8674

// Bug: Since B().a is an rvalue, we tried to treat it like a TARGET_EXPR
// and elide the copy.  But that produces a bitwise copy, which causes us
// to abort in cp_expr_size.

// Test that we actually run the A copy constructor when calling f().

// { dg-do run }

int c;

struct A
{
  A () { ++c; }
  A (const A&) { ++c; }
};

struct B
{
  A a;
};

void f (A) { }

int main ()
{
  f (B().a);
  return c < 2;
}
