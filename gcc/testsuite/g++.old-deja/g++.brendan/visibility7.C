// Build don't link: 
// GROUPS passed visibility
class X
{
public:
  void fn ();// ERROR - .*
};
class Y : private X
{};

class Unrelated
{
public:
  void foo () { Y y; y.fn (); }// ERROR - .*
};
