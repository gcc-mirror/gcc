// Build don't link:

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
  c.D().f(); // ERROR - no matching function
}
