// PR c++/103303
// { dg-do run }
// { dg-additional-options "-ftree-slp-vectorize" }

// A class whose virtual base needs more alignment than the rest of the class
// can be a base subobject at an address that is only aligned to its
// non-virtual alignment; accesses through 'this' must not assume more.

struct alignas(16) A16 { virtual ~A16 () {} long l[2]; };
struct B8 { virtual ~B8 () {} };

struct S : virtual B8, virtual A16
{
  __attribute__((noipa)) S () : v1 (0), v2 (0) {}
  __attribute__((noipa)) void set (long x) { v1 = x; v2 = x; }
  long dummy;
  long v1;
  long v2;
};

struct P { virtual ~P () {} };
struct Pad { long a = 0, b = 0; };

struct D : P, Pad, virtual S { virtual ~D () {} };

static_assert (alignof (S) == 16, "");

int
main ()
{
  D d;
  S *s = &d;
  s->set (42);
  if (s->v1 != 42 || s->v2 != 42)
    __builtin_abort ();
}
