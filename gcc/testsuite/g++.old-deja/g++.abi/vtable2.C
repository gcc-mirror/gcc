// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -fno-strict-aliasing

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100

#include <stddef.h>

struct S0
{
  virtual void s0 ();
};

struct S1 : virtual public S0
{
  virtual void s1 ();
};

struct S2 : virtual public S1
{
  virtual void s1 ();
  virtual void s0 ();
};

struct S3
{
  virtual void s3 ();
};

struct S4 : public S3, virtual public S2
{
  virtual void s1 ();
};

void S0::s0 ()
{
}

void S1::s1 ()
{
}

void S2::s1 ()
{
}

void S2::s0 ()
{
}

void S3::s3 ()
{
}

void S4::s1 ()
{
}

/* The vtables should look like:

   S0 primary vtable
   
     S0 offset to top
     S0 RTTI
     S0::s0

   =================

   S1 primary vtable

     S0::s0 vcall offset
     S0 vbase offset
     S1 offset to top
     S1 RTTI
     S0::s0
     S1::s1

   =================

   S2 primary vtable
   
     S2::s1 vcall offset
     S1 vbase offset
     S2::s0 vcall offset
     S0 vbase offset
     S2 offset to top
     S2 RTTI
     S2::s0
     S2::s1

   =================

   S3 primary vtable

     S3 offset to top
     S3 RTTI
     S3::s3

   =================

   S4 primary vtable

     vbase offset for S0
     vbase offset for S1
     vbase offset for S2
     S4 offset to top
     S4 RTTI
     S3::s3
     S4::s1

   S2-in-S4 secondary vtable

     S1 vbase offset
     S4::s1 vcall offset
     S0 vbase offset
     S2:s0 vcall offset
     S2 offset to top
     S4 RTTI
     S2::s0
     S4::s1

*/

// These are tricks to allow us to get raw function pointers for
// member functions.
extern "C" {
void _ZN2S32s3Ev ();
void _ZN2S42s1Ev ();
}

// IA-64 uses function descriptors not function pointers in its vtables.
#if defined __ia64__
#define CMP_VPTR(A, B)	(*(void **)(A) == *(void **)(B))
#ifdef _LP64
#define INC_VPTR(A)	((A) += 2)
#define INC_VDATA(A,N)	((A) += (N))
#else
#define INC_VPTR(A)	((A) += 4)
#define INC_VDATA(A,N)	((A) += 2*(N))
#endif
#else
#define CMP_VPTR(A, B)	(*(A) == (ptrdiff_t)(B))
#define INC_VPTR(A)	((A) += 1)
#define INC_VDATA(A,N)	((A) += (N))
#endif

int main ()
{
  S4 s4;
  ptrdiff_t **vptr;
  ptrdiff_t *vtbl;

  // Set vtbl to point at the beginning of S4's primary vtable.
  vptr = (ptrdiff_t **) &s4;
  vtbl = *vptr;
  INC_VDATA (vtbl, -5);

  if (*vtbl != ((char*) (S0*) &s4) - (char*) &s4)
    return 1;
  INC_VDATA (vtbl, 1);
  if (*vtbl != ((char*) (S1*) &s4) - (char*) &s4)
    return 2;
  INC_VDATA (vtbl, 1);
  if (*vtbl != ((char*) (S2*) &s4) - (char*) &s4)
    return 3;
  INC_VDATA (vtbl, 1);
  if (*vtbl != 0)
    return 4;
  INC_VDATA (vtbl, 1);
  // Skip the RTTI entry.
  INC_VDATA (vtbl, 1);
  if (! CMP_VPTR (vtbl, &_ZN2S32s3Ev))
    return 5;
  INC_VPTR (vtbl);
  if (! CMP_VPTR (vtbl, &_ZN2S42s1Ev))
    return 6;
  INC_VPTR (vtbl);
  // The S1 vbase offset.
  if (*vtbl != 0)
    return 7;
  INC_VDATA (vtbl, 1);
  // The S4::s1 vcall offset is negative; once you convert to S2, you
  // have to convert to S4 to find the final overrider.
  if (*vtbl != ((char*) &s4 - (char*) (S2*) &s4))
    return 8;
  INC_VDATA (vtbl, 1);
  if (*vtbl != 0)
    return 9;
  INC_VDATA (vtbl, 1);
  if (*vtbl != 0)
    return 10;
  INC_VDATA (vtbl, 1);
  // Now we're at the S2 offset to top entry.
  if (*vtbl != ((char*) &s4 - (char*) (S2*) &s4))
    return 11;
  INC_VDATA (vtbl, 1);
  // Skip the RTTI entry.
  INC_VDATA (vtbl, 1);
  // Skip the remaining virtual functions -- they are thunks.
  INC_VPTR (vtbl);
  INC_VPTR (vtbl);
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main ()
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */
