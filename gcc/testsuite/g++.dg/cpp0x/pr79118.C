// { dg-do compile { target c++11 } }
// { dg-additional-options { -Wno-pedantic } }
// PR c++/79118 failure to check initialization of anonymous members.

struct One
{
  union
  {
    int a;
    int b;
  };

  constexpr One () : a(), b() {} // { dg-error "multiple" }
  constexpr One (int) : a() {}
  constexpr One (unsigned) : b () {}
  constexpr One (void *) {} // { dg-error "exactly one" }
};

One a ();
One b (0);
One c (0u);
One d ((void *)0);

struct Two
{
  struct
  {
    int a;
    int b;
  };

  constexpr Two () : a(), b() {}
  constexpr Two (int) : a() {} // { dg-error "b' must be initialized" }
  constexpr Two (unsigned) : b () {} // { dg-error "a' must be initialized" }
  constexpr Two (void *) {} // { dg-error "a' must be initialized" }
   // { dg-error "b' must be initialized" "" { target *-*-* } .-1 }
};

Two e ();
Two f (0);
Two g (0u);
Two h ((void *)0);
