/* { dg-do run } */
/* { dg-options "-O2" } */

/* e's range is [-1.0, -0.0][1.0, 1.0], which contains -0.0, so e != 0.0
   must not fold to true since -0.0 == 0.0.  */

static double *a;
static void
b (int c)
{
  int d = 1;
  double e = 1.0;
  a = &e;
  while (1)
    {
      if (!(e ? e : 4.0 < -c))
	break;
      e = d - 1;
      *a = -e;
      d = 2;
    }
}

int
main ()
{
  b (0);
  return 0;
}
