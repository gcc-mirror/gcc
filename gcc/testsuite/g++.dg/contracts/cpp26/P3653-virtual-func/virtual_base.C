// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=off -fcontracts-nonattr -fcontracts-on-virtual-functions=P3653 " }
#include <cassert>

struct contract
{
  int checked = 0;
};

contract a, b, c;

bool
checkA ()
{
  a.checked++;
  return true;
}

bool
checkB ()
{
  b.checked++;
  return true;
}

bool
checkC ()
{
  c.checked++;
  return true;
}

void
clear_checks ()
{
  a.checked = b.checked = c.checked = 0;

}

struct Base
{
  virtual void f() pre (checkA ()){};
};


struct Child0 : virtual Base
{
  virtual void f() {};  // inherits checkA
};

struct Child1 : virtual Base
{
  virtual void f() pre (checkB ()){};
};

struct Child2 : virtual Base
{
  virtual void f() pre (checkC ()){};
};


struct GChild0 : Child0
{
  virtual void f() {}; // inherits checkA
};


struct GChild1 : Child0, Child1
{
  virtual void f() pre (checkC ()){};
};

struct GChild2 : Child0, Child1
{
  virtual void f() {}; //inherits checkA and checkB
};


struct GChild3 : Child2, Child1
{
  virtual void f() {}; //inherits checkC and checkB
};



void fooBase(Base* b)
{
    b->f();
}

int main(int, char**)
{
  Base b0;
  Child0 c0;
  Child1 c1;
  GChild0 g0;
  GChild1 g1;
  GChild2 g2;
  GChild3 g3;

  clear_checks ();
  fooBase (&b0);
  assert (a.checked > 0);
  assert (b.checked == 0);
  assert (c.checked == 0);

  clear_checks ();
  fooBase (&c0);
  assert (a.checked > 0);
  assert (b.checked == 0);
  assert (c.checked == 0);

  clear_checks ();
  fooBase (&c1);
  assert (a.checked > 0);
  assert (b.checked > 0);
  assert (c.checked == 0);

  clear_checks ();
  fooBase (&g0);
  assert (a.checked > 0);
  assert (b.checked == 0);
  assert (c.checked == 0);

  clear_checks ();
  fooBase (&g1);
  assert (a.checked > 0);
  assert (b.checked == 0);
  assert (c.checked > 0);

  clear_checks ();
  fooBase (&g2);
  assert (a.checked > 0);
  assert (b.checked > 0);
  assert (c.checked == 0);

  clear_checks ();
  fooBase (&g3);
  assert (a.checked > 0);
  assert (b.checked > 0);
  assert (c.checked > 0);

  clear_checks ();
  b0.f ();
  assert (a.checked > 0);
  assert (b.checked == 0);
  assert (c.checked == 0);

  clear_checks ();
  c0.f ();
  assert (a.checked > 0);
  assert (b.checked == 0);
  assert (c.checked == 0);

  clear_checks ();
  c1.f ();
  assert (a.checked == 0);
  assert (b.checked > 0);
  assert (c.checked == 0);

  clear_checks ();
  g0.f ();
  assert (a.checked > 0);
  assert (b.checked == 0);
  assert (c.checked == 0);

  clear_checks ();
  g1.f ();
  assert (a.checked == 0);
  assert (b.checked == 0);
  assert (c.checked > 0);

  clear_checks ();
  g2.f ();
  assert (a.checked > 0);
  assert (b.checked > 0);
  assert (c.checked == 0);

  clear_checks ();
  g3.f ();
  assert (a.checked == 0);
  assert (b.checked > 0);
  assert (c.checked > 0);

  return 0;
}
