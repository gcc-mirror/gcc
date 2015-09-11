// { dg-do assemble  }
// GROUPS passed visibility
class X
{
public:
  void fn ();// { dg-message "" } .*
};
class Y : private X
{};

class Unrelated
{
public:
  void foo () { Y y; y.fn (); }// { dg-error "" } .*
};
