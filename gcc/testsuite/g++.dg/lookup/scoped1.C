// Test that explicitly scoped references to static members work even if
// they belong to an inaccessible base.

struct A
{
  static int i1;
  int i2; // { dg-message "declared" }
  static void f1 ();
  void f2 ();
};

struct B: private A { };
struct C: public B
{
  void g ()
  {
    ::A::i1 = 1;
    ::A::i2 = 1;		// { dg-error "(access)|(context)" }
    ::A::f1 ();
    ::A::f2 ();			// { dg-error "" }
  }
};
