/* PR tree-optimization/108498 */

struct U { char c[16]; };
struct V { char c[16]; };
struct S { unsigned int a : 3, b : 8, c : 21; struct U d; unsigned int e; struct V f; unsigned int g : 5, h : 27; };
struct T { unsigned int a : 16, b : 8, c : 8; struct U d; unsigned int e; struct V f; unsigned int g : 5, h : 27; };

__attribute__((noipa)) void
foo (struct S *p)
{
  p->b = 231;
  p->c = 42;
  p->d = (struct U) { "abcdefghijklmno" };
  p->e = 0xdeadbeef;
  p->f = (struct V) { "ABCDEFGHIJKLMNO" };
}

__attribute__((noipa)) void
bar (struct S *p)
{
  p->b = 231;
  p->c = 42;
  p->d = (struct U) { "abcdefghijklmno" };
  p->e = 0xdeadbeef;
  p->f = (struct V) { "ABCDEFGHIJKLMNO" };
  p->g = 12;
}

__attribute__((noipa)) void
baz (struct T *p)
{
  p->c = 42;
  p->d = (struct U) { "abcdefghijklmno" };
  p->e = 0xdeadbeef;
  p->f = (struct V) { "ABCDEFGHIJKLMNO" };
  p->g = 12;
}

int
main ()
{
  if (__CHAR_BIT__ != 8 || __SIZEOF_INT__ != 4)
    return 0;
  struct S s = {};
  struct T t = {};
  foo (&s);
  if (s.a != 0 || s.b != 231 || s.c != 42
      || __builtin_memcmp (&s.d.c, "abcdefghijklmno", 16) || s.e != 0xdeadbeef
      || __builtin_memcmp (&s.f.c, "ABCDEFGHIJKLMNO", 16) || s.g != 0 || s.h != 0)
    __builtin_abort ();
  __builtin_memset (&s, 0, sizeof (s));
  s.a = 7;
  s.g = 31;
  s.h = (1U << 27) - 1;
  foo (&s);
  if (s.a != 7 || s.b != 231 || s.c != 42
      || __builtin_memcmp (&s.d.c, "abcdefghijklmno", 16) || s.e != 0xdeadbeef
      || __builtin_memcmp (&s.f.c, "ABCDEFGHIJKLMNO", 16) || s.g != 31 || s.h != (1U << 27) - 1)
    __builtin_abort ();
  __builtin_memset (&s, 0, sizeof (s));
  bar (&s);
  if (s.a != 0 || s.b != 231 || s.c != 42
      || __builtin_memcmp (&s.d.c, "abcdefghijklmno", 16) || s.e != 0xdeadbeef
      || __builtin_memcmp (&s.f.c, "ABCDEFGHIJKLMNO", 16) || s.g != 12 || s.h != 0)
    __builtin_abort ();
  __builtin_memset (&s, 0, sizeof (s));
  s.a = 7;
  s.g = 31;
  s.h = (1U << 27) - 1;
  bar (&s);
  if (s.a != 7 || s.b != 231 || s.c != 42
      || __builtin_memcmp (&s.d.c, "abcdefghijklmno", 16) || s.e != 0xdeadbeef
      || __builtin_memcmp (&s.f.c, "ABCDEFGHIJKLMNO", 16) || s.g != 12 || s.h != (1U << 27) - 1)
    __builtin_abort ();
  baz (&t);
  if (t.a != 0 || t.b != 0 || t.c != 42
      || __builtin_memcmp (&t.d.c, "abcdefghijklmno", 16) || t.e != 0xdeadbeef
      || __builtin_memcmp (&t.f.c, "ABCDEFGHIJKLMNO", 16) || t.g != 12 || t.h != 0)
    __builtin_abort ();
  __builtin_memset (&s, 0, sizeof (s));
  t.a = 7;
  t.b = 255;
  t.g = 31;
  t.h = (1U << 27) - 1;
  baz (&t);
  if (t.a != 7 || t.b != 255 || t.c != 42
      || __builtin_memcmp (&t.d.c, "abcdefghijklmno", 16) || t.e != 0xdeadbeef
      || __builtin_memcmp (&t.f.c, "ABCDEFGHIJKLMNO", 16) || t.g != 12 || t.h != (1U << 27) - 1)
    __builtin_abort ();
  return 0;
}
