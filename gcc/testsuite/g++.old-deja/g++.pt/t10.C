// { dg-do assemble  }

template <class A> class B { public: A a; B(); };
template <> class B<char> { public: int y[10]; };
static B<int> bi;
static B<char> bc;
