/* Test for GNU extensions to compound literals */
/* Origin: Jakub Jelinek <jakub@redhat.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu89" } */

extern void abort (void);
extern void exit (int);

struct A { int i; int j; int k[4]; };
struct B { };

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

int main (void)
{
  if (a.i || a.j != 6 || a.k[0] || a.k[1] || a.k[2] != 12 || a.k[3])
    abort ();
  if (c[0] || c[1] || c[2] != 6 || c[3] != 7 || c[4] != 8)
    abort ();
  if (sizeof (c) != 5 * sizeof (int))
    abort ();
  if (d[0] != 1)
    abort ();
  if (sizeof (d) != sizeof (int))
    abort ();
  exit (0);
}
