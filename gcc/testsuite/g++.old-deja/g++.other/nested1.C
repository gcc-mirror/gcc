// { dg-do assemble  }

struct C
{
  struct D
  {
  };
};

struct E
{
  C& c;
  void g();
};

void E::g()
{
  c.D().f(); // { dg-error "" } no matching function
}
