// PR optimization/11083
// Origin: <nick@ilm.com>
// Reduced testcase by Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// The compiler used to keep unreachable basic blocks after dead edges
// had been purged, which fooled the LCM code of the GCSE pass.

// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions" }

extern void *memmove (void *, const void *, unsigned int) throw ();

struct S {
    int *q;

    S(int *i) : q(i) {}
};

struct X {
    int *p;

    void foo(S first, S last) {
      try        { memmove(0, 0, last.q - first.q); }
      catch(...) { throw; }
    }

   void bar (const X& x);
};

void X::bar (const X& x)
{
  const unsigned int xlen = S(x.p).q - S(x.p).q;

  if (xlen > 0)
    foo(S(x.p), S(x.p));
}
