// PR c++/6073
// This testcase ICEd because finish_struct_bits changed
// A's and const A's TYPE_MODE from QI to BLK, but did
// not change a's DECL_MODE because its mode was not
// TYPE_MAIN_VARIANT.

struct A
{
  static const A a;
  ~A ();
};

void bar (A x);
void foo ();

void
foo ()
{
  bar (A::a);
}
