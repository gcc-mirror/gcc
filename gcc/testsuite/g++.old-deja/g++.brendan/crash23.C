// Build don't link: 
// GROUPS passed old-abort
// This used to die in chainon; it shouldn't any more.

class A
{
public:
  class B {
  public:
    void f ();
    void g (int);
  };
  void B::f () {}// ERROR - .*
  void B::g (int val) {}// ERROR - .*
};
