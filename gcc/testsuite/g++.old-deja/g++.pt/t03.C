// Build don't link: 

template <class A> class B { public: A a; };
static B<int> b_int;
static B<char> b_char;

int foo () { return b_int.a + b_char.a; }
