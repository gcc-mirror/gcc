// Build don't link: 

template <class A>
class B {
public:
  A a;
  B() { a = 2; }
};
static B<int> bi;
