// Test to see that primary bases are selected correctly.
// Origin: Mark Mitchell <mark@codesourcery.com>

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100

// S1 is a nearly-empty base.

struct S1
{
  virtual void f ()
  {
  }
};

// S2 is a dynamic, but not nearly-empty, base.

struct S2
{
  virtual void g ()
  {
  }
    
  int i;
};

// S1 should be the primary base.

struct T1 : public S1, public S2
{
};

// S2 should be the primary base.

struct T2 : public S2, public S1
{
};

// S2 should be the primary base.

struct T3 : virtual public S1, public S2
{
};

// S1 should be the primary base.

struct T4 : virtual public S1, virtual public S2
{
};

// Check that Y is the primary base for X.  Otherwise, return N.
#define CHECK_PRIMARY_BASE(X, Y, N)		\
  {						\
    X x;					\
    if ((void*) &x != (void *) (Y*) (&x))	\
      return N;					\
  }

int main ()
{
  CHECK_PRIMARY_BASE (T1, S1, 1);
  CHECK_PRIMARY_BASE (T2, S2, 2);
  CHECK_PRIMARY_BASE (T3, S2, 3);
  CHECK_PRIMARY_BASE (T4, S1, 4);
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main () 
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */
