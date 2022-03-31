// { dg-do run  }
// { dg-options "-fno-strict-aliasing" }
// Origin: Mark Mitchell <mark@codesourcery.com>

/* Generally, the lowest bit of the ptr is used to indicate whether a
   ptr-to-mem-func points to a virtual or a non-virtual member
   function.  However, some platforms use all bits to encode a
   function pointer.  Such platforms use the lowest bit of the delta,
   that is shifted left by one bit.  */
#if defined __MN10300__ || defined __SH5__ || defined __arm__ || defined __thumb__ || defined __mips__ || defined __aarch64__ || defined __PRU__ || defined __loongarch__
#define ADJUST_PTRFN(func, virt) ((void (*)())(func))
#define ADJUST_DELTA(delta, virt) (((delta) << 1) + !!(virt))
#else
#define ADJUST_PTRFN(func, virt) ((void (*)())((ptrdiff_t)(func) + !!(virt)))
#define ADJUST_DELTA(delta, virt) (delta)
#endif

/* IA64 uses function descriptors instead of function pointers in its
   vtables, which means that we can't meaningfully compare them directly.  */
#if defined __ia64__
#define CMP_PTRFN(A, B)	(*(void **)(A) == *(void **)(B))
#define VPTE_SIZE	(16)
#else
#define CMP_PTRFN(A, B) ((A) == (B))
#define VPTE_SIZE	sizeof(void *)
#endif

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100

// Check that pointers-to-member functions are represented correctly.

#include <cstddef>

struct S
{
  int i;
  int j;
};

// Because S does not have a VPTR, it will not be a primary base of T,
// and will therefore end up at a nonzero offset.

struct T : public S 
{
  void f () {}
  virtual void g () {}
  virtual void h () {}
};

// Provide access to the raw function pointers.  This is
// mangling-dependent.

extern "C" void _ZN1T1fEv ();
extern "C" void _ZN1T1gEv ();
extern "C" void _ZN1T1hEv ();

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
  if (! CMP_PTRFN (yp->ptr, ADJUST_PTRFN (&_ZN1T1fEv, 0)))
    return 5;
  if (yp->adj != ADJUST_DELTA (0, 0))
    return 6;
  x = (sp) y;
  if (! CMP_PTRFN (xp->ptr, ADJUST_PTRFN (&_ZN1T1fEv, 0)))
    return 7;
  if (xp->adj != ADJUST_DELTA (delta, 0))
    return 8;

  // For a virtual function, we should see the vtable offset, plus
  // one.  `T::h' is in the second slot: the vtable pointer points to
  // the first virtual function.
  y = &T::h;
  if (yp->ptr != ADJUST_PTRFN (VPTE_SIZE, 1))
    return 9;
  if (yp->adj != ADJUST_DELTA (0, 1))
    return 10;
  x = (sp) y;
  if (xp->ptr != ADJUST_PTRFN (VPTE_SIZE, 1))
    return 11;
  if (xp->adj != ADJUST_DELTA (delta, 1))
    return 12;

  // Pointers-to-data-members should have the same size and alignment
  // as a ptrdiff_t.
  if (sizeof (sdp) != sizeof (ptrdiff_t))
    return 13;
  if (__alignof__ (sdp) != __alignof__ (ptrdiff_t))
    return 14;

  // The value of a pointer-to-data member should be the offset from
  // the start of the structure.
  sdp z = &S::j;
  if ((char *) &s.j - (char *) &s != *((ptrdiff_t *) &z))
    return 15;
  z = 0;
  if (*((ptrdiff_t *) &z) != -1)
    return 16;
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main () 
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */
