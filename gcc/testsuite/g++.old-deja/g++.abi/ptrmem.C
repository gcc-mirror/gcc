// Special g++ Options: -fno-strict-aliasing
// Origin: Mark Mitchell <mark@codesourcery.com>

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100

// Check that pointers-to-member functions are represented correctly.

#include <cstddef>

struct S
{
  int i;
  int j;
};

// Because S does not have a VPTR, it will not be a primary base of T,
// and will therefore end up at a non-zero offset.

struct T : public S 
{
  void f () {}
  virtual void g () {}
  virtual void h () {}
};

// Provide access to the raw function pointers.  This is
// mangling-dependent.

extern "C" void f__1T ();
extern "C" void g__1T ();
extern "C" void h__1T ();

// This structure is a C representation of a pointer-to-member.

struct ptrmemfunc 
{
  void (*ptr) ();
  ptrdiff_t adj;
};

typedef int S::*sdp;
typedef void (S::*sp)();
typedef void (T::*tp)();

int
main ()
{
  S s;
  T t;
  sp x;
  tp y;
  ptrmemfunc *xp = (ptrmemfunc *) &x;
  ptrmemfunc *yp = (ptrmemfunc *) &y;
  ptrdiff_t delta = ((char *) &t) - ((char*) (S*) (&t));

  // Pointers-to-function-members should have the same size and
  // alignment as the PTRMEMFUNC type.
  if (sizeof (sp) != sizeof (ptrmemfunc))
    return 1;
  if (__alignof__ (sp) != __alignof__ (ptrmemfunc))
    return 2;
  
  // The NULL pointer-to-member should have a NULL first PTR field.
  x = 0;
  if (xp->ptr != 0)
    return 3;
  y = x;
  if (yp->ptr != 0)
    return 4;
  
  // A non-virtual function should have a pointer to the function.
  // There should be no adjustment for the `T' version, and an
  // appropriate adjustment for the `S' version.
  y = &T::f;
  if (yp->ptr != &f__1T)
    return 5;
  if (yp->adj != 0)
    return 6;
  x = (sp) y;
  if (xp->ptr != &f__1T)
    return 7;
  if (xp->adj != delta)
    return 8;

  // For a virtual function, we should see twice the vtable index,
  // plus one.  `T::h' is in the third slot: there's the RTTI entry,
  // then the two virtual functions.
  y = &T::h;
  if ((ptrdiff_t) yp->ptr != 7)
    return 9;
  if (yp->adj != 0)
    return 10;
  x = (sp) y;
  if ((ptrdiff_t) xp->ptr != 7)
    return 11;
  if (xp->adj != delta)
    return 12;

  // Pointers-to-data-members should have the same size and alignment
  // as a ptrdiff_t.
  if (sizeof (sdp) != sizeof (ptrdiff_t))
    return 13;
  if (__alignof__ (sdp) != __alignof__ (ptrdiff_t))
    return 14;

  sdp z = &S::j;
  if ((char *) &s.j - (char *) &s != *((ptrdiff_t *) &z) - 1)
    return 15;
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main () 
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */
