/* { dg-options "-fdump-ipa-inline" } */
// c++/83667 ICE dumping a static thunk

struct a
{
  virtual ~a ();
};

struct b
{
  virtual void d (...);
};

struct c : a, b
{
  void d (...)
  {
  }
};

c c;

// { dg-final { scan-ipa-dump "summary for void c::\\*.LTHUNK0" "inline" } }
