// Build don't link: 

template <class A>
class B {
public:
  A a;
  B() { x = 2; }	// ERROR - no x
};
static B<int> bi;
