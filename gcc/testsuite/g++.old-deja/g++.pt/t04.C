// { dg-do assemble  }

template <class A> class B { public: A a; };
static B<int> b_int;
static B<char> b_char;
static B<unsigned char> b_uchar;

int foo () { return b_int.a + b_char.a + b_uchar.a; }
