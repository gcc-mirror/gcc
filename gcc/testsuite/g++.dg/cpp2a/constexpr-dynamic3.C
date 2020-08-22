// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }

// Sidecast.

struct A {
  virtual void afn () {}
};
 
struct B {
  virtual void bfn () {}
};

struct D : A, B { };

constexpr bool
fn ()
{
  bool ok = true;
  D d;
  A *a = &d;
  if (B *bp = dynamic_cast<B*>(a))
    /*OK*/;
  else
    ok = false;

  A &ar = d;
  B &br = dynamic_cast<B&>(ar);

  return ok;
}

static_assert(fn ());
