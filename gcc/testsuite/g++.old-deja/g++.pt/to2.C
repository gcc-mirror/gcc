// Build don't link: 

template <class A> class B { public: A a; };
static B<int> b_int;
static B<int> b_int2;

int foo () { return b_int.a + b_int2.a; }
