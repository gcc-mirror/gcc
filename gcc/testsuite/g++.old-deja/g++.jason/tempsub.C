// Distillation of crash1.C problem (PR 3633)
// Build don't link:

template<class P>
class A
{
  P p;
};

template<class Q>
class B
{
  A<Q> a;			// bogus error - temp parm name propagating
};

template<class R>
class C
{
  B<R> b;
};

template<class S>
class D
{
  S s;
};

C< D<int> > c;
