/* Copyright (C) 2003, 2004  Free Software Foundation.

   Ensure builtin memmove and bcopy perform correctly.

   Written by Jakub Jelinek, 4/26/2003.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern void *memmove (void *, const void *, size_t);
extern void bcopy (const void *, void *, size_t);
extern int memcmp (const void *, const void *, size_t);

const char s1[] = "123";
char p[32] = "";

static const struct foo
{
  char *s;
  double d;
  long l;
} foo[] =
{
  { "hello world1", 3.14159, 101L },
  { "hello world2", 3.14159, 102L },
  { "hello world3", 3.14159, 103L },
  { "hello world4", 3.14159, 104L },
  { "hello world5", 3.14159, 105L },
  { "hello world6", 3.14159, 106L }
};

static const struct bar
{
  char *s;
  const struct foo f[3];
} bar[] =
{
  {
    "hello world10",
    {
      { "hello1", 3.14159, 201L },
      { "hello2", 3.14159, 202L },
      { "hello3", 3.14159, 203L },
    }
  },
  {
    "hello world11",
    {
      { "hello4", 3.14159, 204L },
      { "hello5", 3.14159, 205L },
      { "hello6", 3.14159, 206L },
    }
  }
};

static const int baz[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 };

void
main_test (void)
{
  const char *s;
  struct foo f1[sizeof foo/sizeof*foo];
  struct bar b1[sizeof bar/sizeof*bar];
  int bz[sizeof baz/sizeof*baz];

  if (memmove (f1, foo, sizeof (foo)) != f1 || memcmp (f1, foo, sizeof (foo)))
    abort ();
  if (memmove (b1, bar, sizeof (bar)) != b1 || memcmp (b1, bar, sizeof (bar)))
    abort ();
  bcopy (baz, bz, sizeof (baz));
  if (memcmp (bz, baz, sizeof (baz)))
    abort ();

  if (memmove (p, "abcde", 6) != p || memcmp (p, "abcde", 6))
    abort ();
  s = s1;
  if (memmove (p + 2, ++s, 0) != p + 2 || memcmp (p, "abcde", 6) || s != s1 + 1)
    abort ();
  if (__builtin_memmove (p + 3, "", 1) != p + 3 || memcmp (p, "abc\0e", 6))
    abort ();
  bcopy ("fghijk", p + 2, 4);
  if (memcmp (p, "abfghi", 7))
    abort ();
  s = s1 + 1;
  bcopy (s++, p + 1, 0);
  if (memcmp (p, "abfghi", 7) || s != s1 + 2)
    abort ();
  __builtin_bcopy ("ABCDE", p + 4, 1);
  if (memcmp (p, "abfgAi", 7))
    abort ();
}
