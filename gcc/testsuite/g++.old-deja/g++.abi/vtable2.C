// Origin: Mark Mitchell <mark@codesourcery.com>

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
void s3__2S3 ();
void s1__2S4 ();
}

int main ()
{
  S4 s4;
  ptrdiff_t **vptr;
  ptrdiff_t *vtbl;

  // Set vtbl to point at the beginning of S4's primary vtable.
  vptr = (ptrdiff_t **) &s4;
  vtbl = *vptr;
  vtbl -= 5;

  if (*vtbl++ != ((char*) (S0*) &s4) - (char*) &s4)
    return 1;
  if (*vtbl++ != ((char*) (S1*) &s4) - (char*) &s4)
    return 2;
  if (*vtbl++ != ((char*) (S2*) &s4) - (char*) &s4)
    return 3;
  if (*vtbl++ != 0)
    return 4;
  // Skip the RTTI entry.
  vtbl++;
  if (*vtbl++ != (ptrdiff_t) &s3__2S3)
    return 5;
  if (*vtbl++ != (ptrdiff_t) &s1__2S4)
    return 6;
  // The S1 vbase offset.
  if (*vtbl++ != 0)
    return 7;
  // The S4::s1 vcall offset is negative; once you convert to S2, you
  // have to convert to S4 to find the final overrider.
  if (*vtbl++ != ((char*) &s4 - (char*) (S2*) &s4))
    return 8;
  if (*vtbl++ != 0)
    return 9;
  if (*vtbl++ != 0)
    return 10;
  // Now we're at the S2 offset to top entry.
  if (*vtbl++ != ((char*) &s4 - (char*) (S2*) &s4))
    return 11;
  // Skip the RTTI entry.
  vtbl++;
  // Skip the remaining virtual functions -- they are thunks.
  vtbl++;
  vtbl++;
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main ()
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */
