/* PR c++/96003 - spurious -Wnonnull calling a member on the result
   of static_cast
   { dg-do compile }
   { dg-options "-Wall" } */

struct D;
struct B
{
  B* next;
  D* Next ();
};

struct D: B
{
  virtual ~D ();
};

struct Iterator
{
  D* p;
  void advance ()
  {
    p = static_cast<B*>(p)->Next ();    // { dg-bogus "\\\[-Wnonnull" }
  }
};

// Test case from comment #11.

struct S1 { virtual ~S1 (); };
struct S2 { virtual ~S2 (); };
struct S3: S1, S2 { void f (); };

void f (S2 *p)
{
  static_cast<S3 *>(p)->f ();           // { dg-bogus "\\\[-Wnonnull" }
}
