// { dg-do assemble  }
// Bug: g++ complains about the use of A::p below.

struct A {
  void *p;
};

struct B: public A {
  int f ()
    {
      if (A::p)
	return 1;
      return 0;
    }
};
