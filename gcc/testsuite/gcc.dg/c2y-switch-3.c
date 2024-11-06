/* C2Y N3370 - Case range expressions.  */
/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic-errors" } */

extern void abort ();

void
foo (int x)
{
  switch (x)
    {
    case -42 ... 42:
      if (x < -42 || x > 42)
        abort ();
      break;
    case 43 ... 43:
      if (x != 43)
        abort ();
      break;
    case 44:
      if (x != 44)
        abort ();
      break;
    case 45 ... 46:
      if (x < 45 || x > 46)
        abort ();
      break;
    default:
      if (x >= -42 && x <= 46)
        abort ();
      break;
    }
}

int
main ()
{
  for (int i = -44; i <= 48; ++i)
    foo (i);
}
