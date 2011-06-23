/* { dg-do run } */
/* { dg-options "-fipa-pta" } */
/* { dg-additional-sources "pr43879-1_0.C" } */

struct A {
    int *i;
    A();
    ~A();
};

static inline int
aa(int *a, int *b)
{
  (void)b;
  return *a;
}

struct B {
    B() : i(0) {}
    int i;
    B(const A &a) : i(0)
    {
      f(*a.i);
    }
    void __attribute__((noinline, noclone))
	f(int j)
	  {
	    aa(&i, &j);
	    i = 1;
	  }
};

int
test()
{
  B b1;
  B b2 = B(A());
  b1 = B(A());
  if (b1.i != b2.i) __builtin_abort();
  return 0;
}

int
main()
{
  return test();
}

