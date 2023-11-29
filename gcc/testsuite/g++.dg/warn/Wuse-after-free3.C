// PR target/104213
// { dg-do compile }
// { dg-options "-Wuse-after-free" }

struct A
{
  virtual ~A ();
  void f ();
};

A::~A ()
{
  operator delete (this);
  f (); // { dg-warning "used after" "" { xfail arm_eabi } }
  // arm_eabi's cdtors return this, which disables -Wuse-after-free
  // warnings for cdtors' "this".
}
