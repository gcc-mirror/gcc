/* PR rtl-optimization/46777 */
/* { dg-do compile } */
/* { dg-options "-fgcse -O -fno-tree-dominator-opts -funroll-loops" } */

struct S { char s[256]; };

static inline int
foo (int x, int y)
{
  switch (x)
    {
    case 1:
    case 2:
      return 3;
    case 3:
    case 4:
      return 2;
    case 5:
      switch (y)
	{
	case 4:
	  return 1;
	}
    }
  return 0;
}

void
bar (struct S *x, int *y, int *z, int **w)
{
  switch (*y ? x->s[*y] : foo (y[1], y[0]))
    {
    case 3:
      if (y + 2 == z)
	for (;;)
	  {
	    y += 2;
	    switch (*y ? x->s[*y] : foo (y[1], y[0]))
	      {
	      case 6:
		break;
	      default:
		*w = y;
	      }
	    if (y == z)
	      break;
	  }
    }
}
