// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++2a } }

// Downcast.

struct B {
  virtual void baz () {}
};
 
struct D : B { };

constexpr bool
fn ()
{
  bool ok = true;
  B b;
  B *b1 = &b;
  if (D *pd = dynamic_cast<D*>(b1))
    ok = false;

  D d;
  B *b2 = &d;
  if (D *pd = dynamic_cast<D*>(b2))
    /*OK*/;
  else
   ok = false;
    
  return ok;
}

static_assert(fn ());

constexpr D d;
constexpr B b;
constexpr B *b1 = const_cast<B*>(&b);
constexpr B *b2 = const_cast<D*>(&d);
static_assert(dynamic_cast<D*>(b2) == &d);
static_assert(&dynamic_cast<D&>(*b2) == &d);
static_assert(dynamic_cast<const B*>(&d) == &d);
static_assert(&dynamic_cast<const B&>(d) == &d);
