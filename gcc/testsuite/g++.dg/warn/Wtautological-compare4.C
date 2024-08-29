// PR c++/116534
// { dg-do compile }
// { dg-options "-Wall" }

template <class A>
struct Test {
    bool foo(unsigned x, unsigned y) {
        bool test = &a[x] == &b[y];
	return test;
    }
    unsigned *a;
    unsigned *b;
};

void
g ()
{
  Test<int> t;
  t.foo (0u, 1u);
  t.foo (0u, 0u);
}
