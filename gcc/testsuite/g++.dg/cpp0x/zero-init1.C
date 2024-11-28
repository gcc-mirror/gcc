// PR c++/117256
// { dg-do run { target c++11 } }
// { dg-options "-O0" }

void *operator new (decltype (sizeof 0), void *p) noexcept { return p; }

struct A { char c; int i; };
#if __cplusplus >= 201402L
struct B { A a; constexpr B (char x, int y) : a () { a.c = x; a.i = y; } };
#else
struct B { A a; B (char x, int y) : a () { a.c = x; a.i = y; } };
#endif

[[gnu::noipa]] void
foo ()
{
  unsigned char buf[sizeof (B)];
  A a1 = A ();
  __builtin_memcpy (buf, &a1, sizeof (buf));
  if (buf[1])
    __builtin_abort ();
  unsigned char m1 alignas (A) [sizeof (A)];
  __builtin_memset (m1, -1, sizeof (m1));
  A *a2 = new (m1) A ();
  __builtin_memcpy (buf, a2, sizeof (*a2));
  if (buf[1])
    __builtin_abort ();
  B b1 (42, -42);
  __builtin_memcpy (buf, &b1, sizeof (b1));
  if (buf[1])
    __builtin_abort ();
  unsigned char m2 alignas (B) [sizeof (B)];
  B *b2 = new (m2) B (1, 2);
  __builtin_memcpy (buf, b2, sizeof (*b2));
  if (buf[1])
    __builtin_abort ();
#if __cplusplus >= 201402L
  constexpr B b3 (3, 4);
  __builtin_memcpy (buf, &b3, sizeof (b3));
  if (buf[1])
    __builtin_abort ();
#endif
}

[[gnu::noipa]] void
bar (unsigned char *p)
{
  (void) p;
}

[[gnu::noipa]] void
baz ()
{
  unsigned char buf[256];
  __builtin_memset (buf, -1, sizeof (buf));
  bar (buf);
}

int
main ()
{
  if (__builtin_offsetof (A, c) == 0
      && __builtin_offsetof (A, i) != 1
      && __builtin_offsetof (B, a) == 0
      && sizeof (A) == sizeof (B))
    {
      baz ();
      foo ();
    }
}
