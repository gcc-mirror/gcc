/* Test for compound literals: in C99 only.  Test for valid uses.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

struct s { int a; int b; };
union u { int c; int d; };

int *i0a = &(int) { 0 };
int *i0b = &(int) { 0 };
int *i1a = &(int) { 1 };
int *i1b = &(int) { 1 };
const int *i0c = &(const int) { 0 };

struct s *s0 = &(struct s) { 1, 2 };
struct s *s1 = &(struct s) { 1, 2 };
const struct s *s2 = &(const struct s) { 1, 2 };

union u *u0 = &(union u) { 3 };
union u *u1 = &(union u) { 3 };
const union u *u2 = &(const union u) { 3 };

int *a0 = (int []) { 1, 2, 3 };
const int *a1 = (const int []) { 1, 2, 3 };

char *p = (char []){ "foo" };

int
main (void)
{
  if (i0a == i0b || i0a == i0c || i0b == i0c)
    abort ();
  if (i1a == i1b)
    abort ();
  if (*i0a != 0 || *i0b != 0 || *i1a != 1 || *i1b != 1 || *i0c != 0)
    abort ();
  *i0a = 1;
  *i1a = 0;
  if (*i0a != 1 || *i0b != 0 || *i1a != 0 || *i1b != 1 || *i0c != 0)
    abort ();
  if (s0 == s1 || s1 == s2 || s2 == s0)
    abort ();
  if (s0->a != 1 || s0->b != 2 || s1->a != 1 || s1->b != 2
      || s2->a != 1 || s2->b != 2)
    abort ();
  s0->a = 2;
  s1->b = 1;
  if (s0->a != 2 || s0->b != 2 || s1->a != 1 || s1->b != 1
      || s2->a != 1 || s2->b != 2)
    abort ();
  if (u0 == u1 || u1 == u2 || u2 == u0)
    abort ();
  if (u0->c != 3 || u1->c != 3 || u2->c != 3)
    abort ();
  u0->d = 2;
  if (u0->d != 2 || u1->c != 3 || u2->c != 3)
    abort ();
  if (a0 == a1)
    abort ();
  if (a0[0] != 1 || a0[1] != 2 || a0[2] != 3
      || a1[0] != 1 || a1[1] != 2 || a1[2] != 3)
    abort ();
  a0[0] = 3;
  if (a0[0] != 3 || a0[1] != 2 || a0[2] != 3
      || a1[0] != 1 || a1[1] != 2 || a1[2] != 3)
    abort ();
  if (p[0] != 'f' || p[1] != 'o' || p[2] != 'o' || p[3] != 0)
    abort ();
  p[0] = 'g';
  if (p[0] != 'g' || p[1] != 'o' || p[2] != 'o' || p[3] != 0)
    abort ();
  if (sizeof((int []) { 1, 2 ,3 }) != 3 * sizeof(int))
    abort ();
  if (sizeof((int []) { [3] = 4 }) != 4 * sizeof(int))
    abort ();
  struct s *y;
  for (int i = 0; i < 3; i++) {
    struct s *x = &(struct s) { 1, i };
    if (x->a != 1 || x->b != i)
      abort ();
    x->a++;
    x->b--;
    if (x->a != 2 || x->b != i - 1)
      abort ();
    if (i && y != x)
      abort ();
    y = x;
  }
  int *z;
  for (int i = 0; i < 4; i++) {
    int *x = (int []){ 0, i, i + 2, i - 3 };
    if (x[0] != 0 || x[1] != i || x[2] != i + 2 || x[3] != i - 3)
      abort ();
    x[0] = x[1];
    x[1] *= x[2];
    x[2] -= x[3];
    x[3] += 7;
    if (x[0] != i || x[1] != i * (i + 2) || x[2] != 5 || x[3] != i + 4)
      abort ();
    if (i && z != x)
      abort ();
    z = x;
  }
  (int) { 0 } = 1;
  (struct s) { 0, 1 }.a = 3;
  (union u) { 3 }.c = 4;
  (int []){ 1, 2 }[0] = 0;
  exit (0);
}
