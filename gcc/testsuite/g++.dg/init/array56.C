/* PR c++/90938 - Initializing array with {1} works, but not {0}
   { dg-do compile { target c++11 } }
   { dg-options "-O -Wall -fdump-tree-optimized" } */

#define assert(e)						\
  ((e) ? (void)0						\
   : (__builtin_printf ("assertion failed on line %i: %s\n",	\
			__LINE__, #e),				\
      __builtin_abort ()))

namespace A {

struct X
{
  X () = default;
  X (int n) : n (n + 1) { }
  int n;
};

static_assert (__is_trivial (X), "X is trivial");

static void test ()
{
  {
    X x[] { 0 };
    assert (1 == x->n);
  }

  {
    X x[1] { 0 };
    assert (1 == x->n);                     // fails
  }

  {
    X x[2] { 0 };
    assert (1 == x[0].n && 0 == x[1].n);    // fails
  }

  {
    X x[] { 1, 0 };
    assert (2 == x[0].n && 1 == x[1].n);    // passes
  }

  {
    X x[2] { 1, 0 };
    assert (2 == x[0].n && 1 == x[1].n);    // fails
  }
}

}

namespace B {

struct X
{
  X () = default;
  X (int *p) : p (p ? p : new int (1)) { }
  int *p;
};

static_assert (__is_trivial (X), "X is trivial");

static void test ()
{
  X x[1] { nullptr };
  assert (*x->p == 1);   // fails

  X y[1] { 0 };
  assert (*y->p == 1);   // fails
}

}

namespace C {

static const char *vector_swizzle (int vecsize, int index)
{
  static const char *swizzle[4][4] =
    {
     { ".x", ".y", ".z", ".w" },
     { ".xy", ".yz", ".zw", nullptr },
     { ".xyz", ".yzw", nullptr, nullptr },
     { "", nullptr, nullptr, nullptr },
    };

  assert (vecsize >= 1 && vecsize <= 4);
  assert (index >= 0 && index < 4);
  assert (swizzle[vecsize - 1][index]);

  return swizzle[vecsize - 1][index];
}

static void test ()
{
  assert (!*vector_swizzle(4, 0));
}

}

int main ()
{
  A::test ();
  B::test ();
  C::test ();
}

// { dg-final { scan-tree-dump-not "abort" "optimized" } }
