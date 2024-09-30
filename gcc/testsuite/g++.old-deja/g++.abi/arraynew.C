// { dg-do run  }
// Origin: Mark Mitchell <mark@codesourcery.com>
// Avoid use of none-overridable new/delete operators in shared
// { dg-options "-static" { target *-*-mingw* } }
/* { dg-skip-if "requires hosted libstdc++ for cstdlib size_t" { ! hostedlib } } */

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100

#include <cstdlib>
#include <new>

void* p;

void* operator new[](size_t s)
#if __cplusplus <= 199711L
  throw (std::bad_alloc)
#endif
{
  // Record the base of the last array allocated.
  p = malloc (s);
  return p;
}

template <typename T>
void check_no_cookie (int i)
{
  void* a = new T[7];
  if (p != a)
    exit (i);
}

template <typename T>
void check_no_placement_cookie (int i)
{
  p = malloc (13 * sizeof (T));
  void* a = new (p) T[13];
  if (p != a)
    exit (i);
}

template <typename T>
void check_cookie (int i)
{
  void* a = new T[11];
  size_t x;
  
  // Compute the cookie location manually.
#ifdef __ARM_EABI__
  x = 8;
#else
  x = __alignof__ (T);
  if (x < sizeof (size_t))
    x = sizeof (size_t);
#endif
  if ((char *) a - x != (char *) p)
    exit (i);

  // Check the cookie value.
  size_t *sp = ((size_t *) a) - 1;
  if (*sp != 11)
    exit (i);

#ifdef __ARM_EABI__
  sp = ((size_t *) a) - 2;
  if (*sp != sizeof (T))
    exit (i);
#endif
}

template <typename T>
void check_placement_cookie (int i)
{
  p = malloc (sizeof (T) * 11 + 100);
  void* a = new (p) T[11];
  size_t x;
  
  // Compute the cookie location manually.
#ifdef __ARM_EABI__
  x = 8;
#else
  x = __alignof__ (T);
  if (x < sizeof (size_t))
    x = sizeof (size_t);
#endif
  if ((char *) a - x != (char *) p)
    exit (i);

  // Check the cookie value.
  size_t *sp = ((size_t *) a) - 1;
  if (*sp != 11)
    exit (i);

#ifdef __ARM_EABI__
  sp = ((size_t *) a) - 2;
  if (*sp != sizeof (T))
    exit (i);
#endif
}

struct X {};

template <typename T>
struct Y { int i; virtual void f () {} };

// A class with a non-trivial destructor -- it needs a cookie.
struct Z { ~Z () {} };
// Likewise, but this class needs a bigger cookie so that the array
// elements are correctly aligned.
struct Z2 { ~Z2 () {} long double d; };
  
struct W1 { void operator delete[] (void *, size_t) {} };
struct W2 { void operator delete[] (void *) {}
            void operator delete[] (void *, size_t) {} };
struct W3 { void operator delete[] (void *, size_t) {}
            void operator delete[] (void *) {} };
struct W4 : public W1 {};

struct V { void *operator new[] (size_t s, void *p) 
             { return p; }
           ~V () {}
         };
   
int main ()
{
  // There should be no cookies for types with trivial destructors.
  check_no_cookie<int> (1);
  check_no_cookie<X> (2);
  check_no_cookie<Y<double> > (3);

  // There should be no cookies for allocations using global placement
  // new.
  check_no_placement_cookie<int> (4);
  check_no_placement_cookie<X> (5);
  check_no_placement_cookie<Z> (6);

  // There should be a cookie when using a non-trivial destructor.
  check_cookie<Z> (7);
  check_cookie<Z2> (8);
  
  // There should be a cookie when using the two-argument array delete
  // operator.
  check_cookie<W1> (9);
  check_cookie<W4> (10);
  // But not when the one-argument version is also available.
  check_no_cookie<W2> (11);
  check_no_cookie<W3> (12);

  // There should be a cookie when using a non-global placement new.
  check_placement_cookie<V> (13);
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main () 
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */
