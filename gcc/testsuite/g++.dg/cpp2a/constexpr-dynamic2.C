// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }

// Downcast, with hint > 0.

struct B {
  virtual void baz () {}
};

struct B2 {
  virtual void baz2 () {}
};
 
struct D : B, B2 { };

constexpr bool
fn ()
{
  // try &/&&, add address test
  bool ok = true;
  B2 b;
  B2 *b1 = &b;
  if (D *pd = dynamic_cast<D*>(b1))
    ok = false;

  D d;
  B2 *b2 = &d;
  if (D *pd = dynamic_cast<D*>(b2))
    /*OK*/;
  else
   ok = false;
    
  return ok;
}

static_assert(fn ());

constexpr D d;
constexpr B2 *b = const_cast<D*>(&d);
static_assert(dynamic_cast<D*>(b) == &d);
static_assert(&dynamic_cast<D&>(*b) == &d);
