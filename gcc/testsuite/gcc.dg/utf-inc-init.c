/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test incremental initializers for char16_t/char32_t arrays. */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

typedef __SIZE_TYPE__ size_t;
typedef short unsigned int char16_t;
typedef unsigned int char32_t;

extern int memcmp (const void *, const void *, size_t);
extern void abort (void);
extern void exit (int);

struct A {
  char16_t S[6];
  int M;
} a[] = { { { u"foo" }, 1 }, [0].S[2] = u'x', [0].S[4] = u'y' };
struct A b[] = { { { u"foo" }, 1 }, [0] = { .S[0] = u'b' } };
struct A c[] = { { { u"foo" }, 1 }, [0].S = { u"a" }, [0].M = 2 };

struct B {
  char32_t S[6];
  int M;
} d[] = { { { U"foo" }, 1 }, [0].S[2] = U'x', [0].S[4] = U'y' };
struct B e[] = { { { U"foo" }, 1 }, [0] = { .S[0] = U'b' } };
struct B f[] = { { { U"foo" }, 1 }, [0].S = { U"a" }, [0].M = 2 };

int main (void)
{
  if (memcmp (a[0].S, u"fox\0y", 6 * sizeof(char16_t)) || a[0].M != 1)
    abort ();
  if (memcmp (b[0].S, u"b\0\0\0\0", 6) || b[0].M)
    abort ();
  if (memcmp (c[0].S, u"a\0\0\0\0", 6) || c[0].M != 2)
    abort ();

  if (memcmp (d[0].S, U"fox\0y", 6 * sizeof(char32_t)) || d[0].M != 1)
    abort ();
  if (memcmp (e[0].S, U"b\0\0\0\0", 6) || e[0].M)
    abort ();
  if (memcmp (f[0].S, U"a\0\0\0\0", 6) || f[0].M != 2)
    abort ();

  exit(0);
}
