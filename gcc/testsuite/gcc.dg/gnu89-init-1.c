/* Test for GNU extensions to compound literals */
/* Origin: Jakub Jelinek <jakub@redhat.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu89" } */

extern void abort (void);
extern void exit (int);

struct A { int i; int j; int k[4]; };
struct B { };
struct C { int i; };
struct D { int i; struct C j; };

/* As a GNU extension, we allow initialization of objects with static storage
   duration by compound literals.  It is handled as if the object
   was initialized only with the bracket enclosed list if compound literal's
   and object types match.  If the object being initialized has array type
   of unknown size, the size is determined by compound literal's initializer
   list, not by size of the compound literal.  */

struct A a = (struct A) { .j = 6, .k[2] = 12 };
struct B b = (struct B) { };
int c[] = (int []) { [2] = 6, 7, 8 };
int d[] = (int [3]) { 1 };
int e[2] = (int []) { 1, 2 };
int f[2] = (int [2]) { 1 };
struct C g[3] = { [2] = (struct C) { 13 }, [1] = (const struct C) { 12 } };
struct D h = { .j = (struct C) { 15 }, .i = 14 };
struct D i[2] = { [1].j = (const struct C) { 17 },
		  [0] = { 0, (struct C) { 16 } } };
struct C j[2][3] = { [0 ... 1] = { [0 ... 2] = (struct C) { 26 } } };
struct C k[3][2] = { [0 ... 2][0 ... 1] = (const struct C) { 27 } };

int main (void)
{
  if (a.i || a.j != 6 || a.k[0] || a.k[1] || a.k[2] != 12 || a.k[3])
    abort ();
  if (c[0] || c[1] || c[2] != 6 || c[3] != 7 || c[4] != 8)
    abort ();
  if (sizeof (c) != 5 * sizeof (int))
    abort ();
  if (d[0] != 1 || d[1] || d[2])
    abort ();
  if (sizeof (d) != 3 * sizeof (int))
    abort ();
  if (e[0] != 1 || e[1] != 2)
    abort ();
  if (sizeof (e) != 2 * sizeof (int))
    abort ();
  if (f[0] != 1 || f[1])
    abort ();
  if (sizeof (f) != 2 * sizeof (int))
    abort ();
  if (g[0].i || g[1].i != 12 || g[2].i != 13)
    abort ();
  if (h.i != 14 || h.j.i != 15)
    abort ();
  if (i[0].i || i[0].j.i != 16 || i[1].i || i[1].j.i != 17)
    abort ();
  if (j[0][0].i != 26 || j[0][1].i != 26 || j[0][2].i != 26)
    abort ();
  if (j[1][0].i != 26 || j[1][1].i != 26 || j[1][2].i != 26)
    abort ();
  if (k[0][0].i != 27 || k[0][1].i != 27 || k[1][0].i != 27)
    abort ();
  if (k[1][1].i != 27 || k[2][0].i != 27 || k[2][1].i != 27)
    abort ();
  exit (0);
}
