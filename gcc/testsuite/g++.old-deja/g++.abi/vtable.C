// { dg-do run  }
// { dg-options "-fno-strict-aliasing" }
// Test various aspects of vtable layout.
// Origin: Mark Mitchell <mark@codesourcery.com>

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100

struct S0
{
  virtual void h ()
  {
  }

  int k;
};


struct S1 
{
  virtual void f () 
  {
  }

  int i;
};

struct S2 : virtual public S0
{
  virtual void g ()
  {
  }

  int j;
};

struct S3 
{
  virtual void k () 
  {
  }

  int l;
};

struct S4 : public virtual S1, public S2, public S3
{
};

inline void* vtable (void *object)
{
  // The vptr is always the first part of the object.
  return * (void **) object;
}

int main ()
{
  // The vtable layout order for S4 should consist of S4's primary
  // vtable (shared with S2), followed by the vtable for S3 (because
  // it is a non-virtual base).  Then, these should be followed by the
  // the vtables for S1 and S0, which are virtual.
  S4 s4;
  S0 *s0 = &s4;
  S1 *s1 = &s4;
  S2 *s2 = &s4;
  S3 *s3 = &s4;
  
  if (vtable (&s4) != vtable (s2))
    return 1;
  if (vtable (s2) >= vtable (s3))
    return 2;
  if (vtable (s3) >= vtable (s1))
    return 3;
  if (vtable (s1) >= vtable (s0))
    return 4;
}

#else /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */

int main () 
{
}

#endif /* !(defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100) */
