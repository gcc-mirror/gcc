// g++ should not complain about A having private [cd]tors.

class A
{
  A();
  ~A();
public:
  int dummy();			// needed to get bogus warning
  static A* get_A ();
};

A* A::get_A()
{
  static A a;
  return &a;
}
