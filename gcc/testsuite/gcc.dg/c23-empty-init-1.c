/* Test C23 support for empty initializers: valid use cases.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

extern void exit (int);
extern void abort (void);

struct s { int a; };
struct s s = {};
int x = {};
float y = {};
void *p = {};
union u { int a; long b; };
union u z = {};
int aa[2] = {};

void
f (int a)
{
  volatile int vla[a] = {};
  struct s as = {};
  int ax = {};
  float ay = {};
  void *ap = {};
  union u az = {};
  int aaa[2] = {};
  for (int i = 0; i < a; i++)
    if (vla[i] != 0)
      abort ();
  if (as.a != 0)
    abort ();
  if (ax != 0)
    abort ();
  if (ay != 0)
    abort ();
  if (ap != 0)
    abort ();
  if (az.a != 0)
    abort ();
  if (aaa[0] != 0)
    abort ();
  if (aaa[1] != 0)
    abort ();
  if ((int) {} != 0)
    abort ();
  if ((float) {} != 0)
    abort ();
  if ((struct s) {}.a != 0)
    abort ();
  if ((union u) {}.a != 0)
    abort ();
  if ((int [5]) {}[2] != 0)
    abort ();
  /* Overwrite contents of vla before second call to make it more likely stack
     contents are nonzero if proper initialization did not occur.  */
  for (int i = 0; i < a; i++)
    vla[i] = -1;
}

int
main (void)
{
  f (100);
  f (100);
  if (s.a != 0)
    abort ();
  if (x != 0)
    abort ();
  if (y != 0)
    abort ();
  if (p != 0)
    abort ();
  if (z.a != 0)
    abort ();
  if (aa[0] != 0)
    abort ();
  if (aa[1] != 0)
    abort ();
  exit (0);
}
