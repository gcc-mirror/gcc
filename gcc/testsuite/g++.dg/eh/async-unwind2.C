// PR rtl-optimization/36419
// { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
// { dg-require-effective-target fpic }
// { dg-options "-Os -fasynchronous-unwind-tables -fpic -fno-inline" }

#include <stdarg.h>

extern "C" void abort ();

extern "C"
{
  struct R { int r1; unsigned short r2[1]; };
  int bar1 (unsigned short *, int, short) throw ();
  void bar2 (R *) throw ();
  void bar3 (R **, const unsigned short *, int) throw ();
  void bar4 (R **, const char *) throw ();
  void bar5 (void *, const char *, ...);
}

struct S
{
  R *s;
  struct T { };
  S (R *x, T *) { s = x; }
  ~S () { bar2 (s); }
  S &operator= (const S &x);
  S &operator+= (const S &x);
  S sfn1 (const S &x) const;
  friend S operator+ (const S &x1, const S &x2);
  static S sfn2 (int i)
  {
    unsigned short q[33];
    R *p = 0;
    bar3 (&p, q, bar1 (q, i, 10));
    return S (p, (T *) 0);
  }
  static S sfn3 (const char *x)
  {
    R *p = 0;
    bar4 (&p, x);
    return S (p, (T *) 0);
  }
};

struct U { };
template <class C> unsigned char operator >>= (const U &, C &);

struct V;
struct W
{
  V *w;
  unsigned char is () const;
};

template <class T> struct X : public W
{
  inline ~X ();
  X ();
  X (const W &);
  T *operator -> () const;
};

struct E
{
  E ();
  E (const S &, const X <V> &);
  E (E const &);
  ~E ();
  E &operator = (E const &);
};

struct V
{
  virtual void release () throw ();
};

template <class T> X <T>::~X ()
{
  if (w)
    w->release ();
}

struct Y
{
  virtual U yfn1 (const S &);
};

struct Z;

X <V> baz1 (const S &)
#if __cplusplus <= 201402L
throw (E)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
;
X <Z> baz2 (const X <Z> &)
#if __cplusplus <= 201402L
throw (E)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
;

template <typename T> X<T>::X ()
{
  w = __null;
}

template <typename T> X<T>::X (W const &)
{
  w = __null;
}

U Y::yfn1 (const S &)
{
  throw 12;
}

Y y;

template <typename T> T *X<T>::operator -> () const
{
  return &y;
}

X <V> baz1 (const S &)
#if __cplusplus <= 201402L
throw (E)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
{
  return X<V> ();
}

E::E ()
{
}

E::~E ()
{
}

X <Z> baz2 (const X <Z> &)
#if __cplusplus <= 201402L
throw (E)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
{
  throw E ();
}

int bar1 (unsigned short *, int, short) throw ()
{
  asm volatile ("" : : : "memory");
  return 0;
}

void bar2 (R *) throw ()
{
  asm volatile ("" : : : "memory");
}

void bar3 (R **, const unsigned short *, int) throw ()
{
  asm volatile ("" : : : "memory");
}

void bar4 (R **, const char *) throw ()
{
  asm volatile ("" : : : "memory");
}

int events[2];
void *sp;

void bar5 (void *p, const char *s, ...)
{
  va_list ap;
  va_start (ap, s);
  if (p)
    throw 19;
  switch (*s)
    {
    case 't':
      if (events[0] != va_arg (ap, int))
	abort ();
      events[0]++;
      break;
    case 'f':
      abort ();
    case 'c':
      if (events[1] != va_arg (ap, int))
	abort ();
      events[1]++;
      if (events[1] == 1)
	sp = va_arg (ap, void *);
      else if (sp != va_arg (ap, void *))
	abort ();
      break;
    }
}

unsigned char W::is () const
{
  return 1;
}

S &S::operator += (const S &)
{
  return *this;
}

template <class C> unsigned char operator >>= (const U &, C &)
{
  throw 1;
}

template X<Y>::X ();
template X<Z>::X ();
template unsigned char operator >>= (const U &, X<Z> &);
template X<Y>::X (W const &);

template Y *X<Y>::operator-> () const;

X <Z> foo () throw ()
{
  X <Z> a;
  X <Y> b;
  try
  {
    b = X <Y> (baz1 (S::sfn3 ("defg")));
  }
  catch (E &)
  {
  }
  if (b.is ())
    {
      for (int n = 0; n < 10; n++)
	{
	  S c = S::sfn3 ("abcd");
	  c += S::sfn2 (n);
	  X <Z> d;
	  try
	  {
	    bar5 ((void *) 0, "trying %d\n", n);
	    if ((b->yfn1 (c) >>= d))
	      if (d.is ())
		{
		  bar5 ((void *) 0, "failure1 on %d\n", n);
		  a = baz2 (d);
		  if (a.is ())
		    break;
		}
	      bar5 ((void *) 0, "failure2 on %d\n", n);
	  }
	  catch (...)
	  {
	    void *p;
	    asm volatile ("movl %%esp, %0" : "=r" (p));
	    bar5 ((void *) 0, "caught %d %p\n", n, p);
	  }
	}
    }
  return a;
}

int
main ()
{
  foo ();
  if (events[0] != 10 || events[1] != 10)
    abort ();
  return 0;
}
