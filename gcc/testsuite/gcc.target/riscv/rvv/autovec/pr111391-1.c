/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -Wno-int-conversion -Wno-implicit-function -Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Ofast -ftree-vectorize" } */

int d ();
typedef struct
{
  int b;
} c;
int
e (char *f, long g)
{
  f += g;
  while (g--)
    *--f = d;
}

int
d (c * f)
{
  while (h ())
    switch (f->b)
      case 'Q':
      {
	long a;
	e (&a, sizeof (a));
	i (a);
      }
}
