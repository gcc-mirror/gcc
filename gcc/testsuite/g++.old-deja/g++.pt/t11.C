// { dg-do assemble  }

template <class A>
class B {
public:
  A a;
  B() { x = 2; }	// { dg-error "" } no x
};
static B<int> bi;
