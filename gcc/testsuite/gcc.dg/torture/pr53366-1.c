/* PR tree-optimization/53366 */
/* { dg-do run { target { size32plus } } } */

extern void abort (void);

struct S { double v[3]; };
struct T { struct S r, i; };
struct U { struct T j[5]; };

void
foo (struct U *__restrict p1, struct U *__restrict p2,
     struct S l1, struct S l2, struct S l3, struct S l4,
     const double _Complex * __restrict x, int y, int z)
{
  int i, j;
  while (y < z - 2)
    {
      for (j = 0; j < 5; ++j)
	{
	  double a = __real__ x[5 * y + j];
	  double b = __imag__ x[5 * y + j];
	  double c = __real__ x[5 * (y + 2) + j];
	  double d = __imag__ x[5 * (y + 2) + j];
	  double e = __real__ x[5 * (y + 1) + j];
	  double f = __imag__ x[5 * (y + 1) + j];
	  double g = __real__ x[5 * (y + 3) + j];
	  double h = __imag__ x[5 * (y + 3) + j];
	  for (i = 0; i < 3; ++i)
	    {
	      p1->j[j].r.v[i] += l2.v[i] * a;
	      p1->j[j].r.v[i] += l4.v[i] * c;
	      p1->j[j].i.v[i] += l2.v[i] * b;
	      p1->j[j].i.v[i] += l4.v[i] * d;
	      p2->j[j].r.v[i] += l3.v[i] * e;
	      p2->j[j].r.v[i] += l1.v[i] * g;
	      p2->j[j].i.v[i] += l3.v[i] * f;
	      p2->j[j].i.v[i] += l1.v[i] * h;
	    }
	}
      y += 4;
    }
}

_Complex double x[5005];
struct U p1, p2;

int
main ()
{
  int i, j;
  struct S l1, l2, l3, l4;
  for (i = 0; i < 5005; ++i)
    x[i] = i + 1.0iF * (2 * i);
  for (i = 0; i < 3; ++i)
    {
      l1.v[i] = 1;
      l2.v[i] = 2;
      l3.v[i] = 3;
      l4.v[i] = 4;
    }
  foo (&p1, &p2, l1, l2, l3, l4, x, 5, 1000);
  for (j = 0; j < 5; ++j)
    for (i = 0; i < 3; ++i)
      if (p1.j[j].r.v[i] != 3752430 + j * 1494.0
	  || p1.j[j].i.v[i] != p1.j[j].r.v[i] * 2
	  || p2.j[j].r.v[i] != 2502450 + j * 996.0
	  || p2.j[j].i.v[i] != p2.j[j].r.v[i] * 2)
	abort ();
  return 0;
}
