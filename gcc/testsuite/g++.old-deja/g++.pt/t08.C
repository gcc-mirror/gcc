// Build don't link: 

template <class A> class B {
  A a;
 public:
  B ();
  ~B ();
};
B<int> b_int;
B<int> *bp = &b_int;
