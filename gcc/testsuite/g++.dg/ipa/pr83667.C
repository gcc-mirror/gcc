// { dg-require-alias "" }
// { dg-options "-fdump-ipa-inline" }
// c++/83667 ICE dumping a static thunk when TARGET_USE_LOCAL_THUNK_ALIAS_P


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

// { dg-final { scan-ipa-dump "summary for void c::\[^\\n\]*THUNK\\.*0" "inline" } }
