/* Test for C99 designated initializers */
/* Origin: Jakub Jelinek <jakub@redhat.com> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

typedef __SIZE_TYPE__ size_t;
typedef __WCHAR_TYPE__ wchar_t;
extern int memcmp (const void *, const void *, size_t);
extern void abort (void);
extern void exit (int);

int a[10] = { 10, 0, 12, 13, 14, 0, 0, 17, 0, 0 };
int b[10] = { 10, [4] = 15, [2] = 12, [4] = 14, [7] = 17 };
int c[10] = { 10, [4] = 15, [2] = 12, [3] = 13, 14, [7] = 17 };
struct A {
  int B;
  short C[2];
};
struct A d[] = { { 0, { 1, 2 } }, { 0, { 0, 0 } }, { 10, { 11, 12 } } };
struct A e[] = { 0, 1, 2, [2] = 10, 11, 12 };
struct A f[] = { 0, 1, 2, [2].C = 11, 12, 13 };
struct A g[] = { 0, 1, 2, [2].C[1] = 12, 13, 14 };
struct A h[] = { 0, 1, 2, [2] = { .C[1] = 12 }, 13, 14 };
struct A i[] = { 0, 1, 2, [2] = { .C = { [1] = 12 } }, 13, 14 };
union D {
  int E;
  double F;
  struct A G;
};
union D j[] = { [4] = 1, [4].F = 1.0, [1].G.C[1] = 4 };
struct H {
  char I[6];
  int J;
} k[] = { { { "foo" }, 1 }, [0].I[0] = 'b' };
struct K {
  wchar_t L[6];
  int M;
} l[] = { { { L"foo" }, 1 }, [0].L[2] = L'x', [0].L[4] = L'y' };
struct H m[] = { { { "foo" }, 1 }, [0] = { .I[0] = 'b' } };
struct H n[] = { { { "foo" }, 1 }, [0].I = { "a" }, [0].J = 2 };
int o = { 22 };

int main (void)
{
  if (b[3])
    abort ();
  b[3] = 13;
  if (memcmp (a, b, sizeof (a)) || memcmp (a, c, sizeof (a)))
    abort ();
  if (memcmp (d, e, sizeof (d)) || sizeof (d) != sizeof (e))
    abort ();
  if (f[2].B != 0 || g[2].B != 0 || g[2].C[0] != 0)
    abort ();
  if (memcmp (g, h, sizeof (g)) || memcmp (g, i, sizeof (g)))
    abort ();
  f[2].B = 10;
  g[2].B = 10;
  g[2].C[0] = 11;
  if (memcmp (d, f, sizeof (d)) || memcmp (d, g, sizeof (d)))
    abort ();
  if (f[3].B != 13 || g[3].B != 13 || g[3].C[0] != 14)
    abort ();
  if (j[0].E || j[1].G.B || j[1].G.C[0] || j[1].G.C[1] != 4)
    abort ();
  if (j[2].E || j[3].E || j[4].F != 1.0)
    abort ();
  if (memcmp (k[0].I, "boo\0\0", 6) || k[0].J != 1)
    abort ();
  if (memcmp (l[0].L, L"fox\0y", 6 * sizeof(wchar_t)) || l[0].M != 1)
    abort ();
  if (memcmp (m[0].I, "b\0\0\0\0", 6) || m[0].J)
    abort ();
  if (memcmp (n[0].I, "a\0\0\0\0", 6) || n[0].J != 2)
    abort ();
  if (o != 22)
    abort ();
  exit (0);
}
