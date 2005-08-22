// PR rtl-optimization/23478
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort ();
bool tthrow;
struct C3 { int i; };
class C14 {};
struct C7
{
  virtual ~C7 ();
};

C7::~C7 ()
{
  asm volatile ("" : : : "memory");
}
class C2 : public C7 {};

template <class X> class C13
{
  bool ma;
  X *mb;
public:
  explicit C13 (X *p = 0) throw () : ma (p != 0), mb (p) {}
  ~C13 ();
};

template <class X>
C13<X>::~C13 ()
{
  asm volatile ("" : : "r" (ma), "r" (mb) : "memory");
}

struct C1
{
  C1 (const C3 &, const C3 &, const C3 &, const C3 *&);
};

C1::C1 (const C3 &, const C3 &, const C3 &, const C3 *&)
{
  if (!tthrow)
    throw 24;
}

struct C8
{
  struct C15 {};
  typedef C15 *C9;
  virtual void f1 (C2 &, long *, void *, C3 &, void *, bool) = 0;
  virtual C13<C14> f3 () const = 0;
  virtual ~C8 () {}
};

bool
xx14 ()
{
  bool b = false;
  if (tthrow)
    throw 6;
  asm volatile ("" : : "r" (&b) : "memory");
  return b;
}

bool
xx2 ()
{
  bool b = false;
  if (tthrow)
    throw 6;
  asm volatile ("" : : "r" (&b) : "memory");
  return b;
}

C13<C7>
xx9 ()
{
  return C13<C7>();
}

C2 &
xx10 ()
{
  static C2 c2;
  return c2;
}

C3 &
xx12 ()
{
  static C3 c3 = { 1 };
  return c3;
}

const C3 &
xx5 ()
{
  static const C3 c3 = { 2 };
  return c3;
}

const C3 *&
xx4 ()
{
  static const C3 *p;
  if (tthrow)
    throw 6;
  return p;
}

long ll13;

long
xx13 ()
{
  long ret;
  asm volatile ("" : "=r" (ret) : "r" (ll13));
  return ret;
}

void
xx15 (C3 &x, C13<C1> &y)
{
  asm volatile ("" : : "r" (&x), "r" (&y) : "memory");
}

long
xx16 (const void *x)
{
  long ret;
  asm volatile ("" : "=r" (ret) : "0" (1), "r" (x) : "memory");
  return ret;
}

void
xx1 (C13<C14> x)
{
  asm volatile ("" : : "r" (&x) : "memory");
  if (tthrow)
    throw 6;
}

void
xx3 (const C7 *x)
{
  if (x)
    abort ();
}

void
xx7 ()
{
  asm volatile ("" : : : "memory");
}

struct C5
{
  C13<C7> f2 (C3 &v1, const void *v2, C8 *v6);
  C7 *m2[2];
  long m1[2];
};

C13<C7>
C5::f2 (C3 &v1, const void *v2, C8 *v6)
{
  C13<C7> v13 = xx9 ();
  C2 &v9 = xx10 ();
  for (long i = 1; i < 2; i++)
    xx3 (m2[i]);
  const C3 &ld = xx5 ();
  xx7 ();
  if (xx2 ())
    throw "";
  xx4 ();
  C3 &si = xx12 ();
  for (long i = 0; i < xx16 (v2); ++i)
    {
      C13<C1> sk (new C1 (xx5 (), ld, xx5 (), xx4 ()));
      xx15 (si, sk);
    }
  long v4 = xx13 ();
  for (long i = v4 - 1; i >= 0; --i)
    m1[i] = i;
  bool v8 = xx2 ();
  for (long i = 0; i < 2 && !xx14 (); i++)
    {
      v6[i].f1 (v9, 0, __null, v1, __null, v8);
      if (v8)
	xx1 (v6[i].f3 ());
    }
  return v13;
}

int
main (void)
{
  C5 c5 = { { __null, __null }, { 0, 0 } };
  bool seen = false;
  try
    {
      c5.f2 (xx12 (), __null, __null);
    }
  catch (int n)
    {
      if (n != 24)
	abort ();
      seen = true;
    }
  if (!seen)
    abort ();
}
