// { dg-do assemble  }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Apr 2001 <nathan@codesourcery.com>

// Check we deal with trailing empty base classes properly

struct A {};
struct B1 : A {};
struct B2 : A {};
struct B3 : A {};
struct B4 : A {};
struct B5 : A {};
struct B6 : A {};
struct B7 : A {};
struct B8 : A {};

struct C1 : B1
{
  virtual void Foo () {};
};
struct C2 : B1, B2
{
  virtual void Foo () {};
};
struct C3 : B1, B2, B3
{
  virtual void Foo () {};
};
struct C4 : B1, B2, B3, B4
{
  virtual void Foo () {};
};
struct C5 : B1, B2, B3, B4, B5
{
  virtual void Foo () {};
};
struct C6 : B1, B2, B3, B4, B5, B6
{
  virtual void Foo () {};
};
struct C7 : B1, B2, B3, B4, B5, B6, B7
{
  virtual void Foo () {};
};
struct C8 : B1, B2, B3, B4, B5, B6, B7, B8
{
  virtual void Foo () {};
};

struct D1 : virtual C1 {};
struct D2 : virtual C2 {};
struct D3 : virtual C3 {};
struct D4 : virtual C4 {};
struct D5 : virtual C5 {};
struct D6 : virtual C6 {};
struct D7 : virtual C7 {};
struct D8 : virtual C8 {};

unsigned const nearly_empty_size = sizeof (D1);

template <typename Cn, typename Dn> int Check (Dn const &ref)
{
  if ((sizeof (Cn) <= nearly_empty_size)
      != (static_cast <void const *> (&ref)
	  == static_cast <Cn const *> (&ref)))
    return 1;
  return 0;
}

template <typename Bn, typename Cn> int Check ()
{
  Cn c[2];

  if (static_cast <A *> (static_cast <B1 *> (&c[1]))
      == static_cast <A *> (static_cast <Bn *> (&c[0])))
    return 1;
  return 0;
}

  
int main ()
{
#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
  if (Check<B1, C1> ())
    return 1;
  if (Check<B2, C2> ())
    return 2;
  if (Check<B3, C3> ())
    return 3;
  if (Check<B4, C4> ())
    return 4;
  if (Check<B5, C5> ())
    return 5;
  if (Check<B6, C6> ())
    return 6;
  if (Check<B7, C7> ())
    return 7;
  if (Check<B8, C8> ())
    return 8;
  
  if (Check<C1> (D1 ()))
    return 11;
  if (Check<C2> (D2 ()))
    return 12;
  if (Check<C3> (D3 ()))
    return 13;
  if (Check<C4> (D4 ()))
    return 14;
  if (Check<C5> (D5 ()))
    return 15;
  if (Check<C6> (D6 ()))
    return 16;
  if (Check<C7> (D7 ()))
    return 17;
  if (Check<C8> (D8 ()))
    return 18;
  
  if (sizeof (C2) == nearly_empty_size)
    return 22;
  if (sizeof (C3) == nearly_empty_size)
    return 23;
  if (sizeof (C4) == nearly_empty_size)
    return 24;
  if (sizeof (C5) == nearly_empty_size)
    return 25;
  if (sizeof (C6) == nearly_empty_size)
    return 26;
  if (sizeof (C7) == nearly_empty_size)
    return 27;
  if (sizeof (C8) == nearly_empty_size)
    return 28;
#endif
  return 0;
  
}
