// { dg-do run }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Jul 2001 <nathan@codesourcery.com>

// Bug 3820. We were bit copying empty bases including the
// padding. Which clobbers whatever they overlay.

struct Empty {};

struct Inter : Empty {};

long now = 0;

struct NonPod
{
  long m;

  NonPod () {m = 0x12345678;}
  NonPod (long m_) {m = m_;}
  NonPod &operator= (NonPod const &src) {now = m; m = src.m; return *this;}
  NonPod (NonPod const &src) {m = src.m;}
};

struct A : Inter
{
  A (long c) {m = c;}
  
  NonPod m;
};

struct B
{
  Inter empty;
  NonPod m;

  B (long c) {m = c;}
};

#if __cpp_attributes
struct B2
{
  [[no_unique_address]] Inter empty;
  NonPod m;

  B2 (long c) {m = c;}
};
#endif

struct C : NonPod, Inter
{
  C (long c) : NonPod (c), Inter () {}
};

int main ()
{
  A a (0x12131415);
  
  long was = a.m.m;
  
  a = 0x22232425;

  if (was != now)
    return 1;	// we copied the empty base which clobbered a.m.m's
		// original value.
  
  A b (0x32333435);
  *(Inter *)&a = *(Inter *)&b;
  
  if (a.m.m != 0x22232425)
    return 2;	// we copied padding, which clobbered a.m.m

  A b2 (0x32333435);
  (Inter &)b2 = Inter ();
  if (b2.m.m != 0x32333435)
    return 2;	// we copied padding, which clobbered b2.m.m
  
  {
  B c (0x12131415);
  was = c.m.m;
  c = 0x22232425;
  if (was != now)
    return 3;
  
  B d (0x32333435);
  c.empty = d.empty;

  if (c.m.m != 0x22232425)
    return 4;
  }
#if __cpp_attributes
  {
  B2 c (0x12131415);
  was = c.m.m;
  c = 0x22232425;
  if (was != now)
    return 3;
  
  B2 d (0x32333435);
  c.empty = d.empty;

  if (c.m.m != 0x22232425)
    return 4;
  }    
#endif

  C e (0x32333435);

  if (e.m != 0x32333435)
    return 2;	// we copied padding
  
  return 0;
}
