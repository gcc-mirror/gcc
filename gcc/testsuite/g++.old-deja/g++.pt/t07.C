// { dg-do assemble  }

template <class A> class B {
  A a;
 public:
  const A& value () { return a; }
};
static B<int> b_int;

int foo () { return b_int.value(); }
