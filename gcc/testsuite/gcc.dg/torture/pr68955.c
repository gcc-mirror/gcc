/* PR rtl-optimization/68955 */
/* { dg-do run } */
/* { dg-output "ONE1ONE" } */

int a, b, c, d, g, m;
int i[7][7][5] = { { { 5 } }, { { 5 } },
		   { { 5 }, { 5 }, { 5 }, { 5 }, { 5 }, { -1 } } };
static int j = 11;
short e, f, h, k, l;

static void
foo ()
{
  for (; e < 5; e++)
    for (h = 3; h; h--)
      {
	for (g = 1; g < 6; g++)
	  {
	    m = c == 0 ? b : b / c;
	    i[e][1][e] = i[1][1][1] | (m & l) && f;
	  }
	for (k = 0; k < 6; k++)
	  {
	    for (d = 0; d < 6; d++)
	      i[1][e][h] = i[h][k][e] >= l;
	    i[e + 2][h + 3][e] = 6 & l;
	    i[2][1][2] = a;
	    for (; j < 5;)
	      for (;;)
		;
	  }
      }
}

int
main ()
{
  foo ();
  __builtin_printf ("ONE%dONE\n", i[1][0][2]);
  return 0;
}
