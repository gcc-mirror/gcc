/* PR tree-optimization/95857 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct E { int e; };
int bar (void), baz (void);
void qux (void *);

void
foo (int x)
{
  struct E a = { 0 };
  struct E i = { 0 };
  qux (&&lab2);
  if (baz ())
    i.e = 1;
  else
    a.e = -2;
  switch (a.e)
    {
    case -2:
    lab1:
      switch (i.e)
	{
	case -3:
	case 2:
	  if (i.e-- != 2)
	    __builtin_unreachable ();
	lab2:
	  baz ();
	  goto lab1;
	case 0:
	  bar ();
	}
      break;
    }
}
