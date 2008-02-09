// PR c++/35116
// Test that break_out_target_exprs works properly with complex
// value-initialization.

struct A
{
  virtual void f ();
};

struct B
{
  A a;
};

struct C
{
  C (int, B = B());
};

void f ()
{
  C c (4);
}
