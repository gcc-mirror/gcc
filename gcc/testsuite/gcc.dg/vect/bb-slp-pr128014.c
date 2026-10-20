/* { dg-do compile } */

typedef unsigned long v2du __attribute__ ((vector_size (16)));
extern int rt (void) __attribute__ ((returns_twice));
extern v2du getv (void);
extern void use (unsigned long);

void
f (v2du *pv)
{
  rt ();
  v2du v = getv ();
  unsigned long a0 = v[0], a1 = v[1];
  pv[1] = (v2du) { a0 ^ 5, a1 ^ 5 };
  use (a1);
  rt ();
}
