// PR c++/93310
// { dg-do compile { target c++20 } }

struct A
{
  virtual constexpr char f () const
  { return 'A'; }
};

struct B : A
{
  char x;

  constexpr B () : x (0)
  { x = ((A *)this)->f(); }

  virtual constexpr char f () const
  { return 'B'; }
};

struct C : B
{
  virtual constexpr char f () const
  { return 'C'; }
};

constexpr C c;
static_assert (c.x == 'B');
