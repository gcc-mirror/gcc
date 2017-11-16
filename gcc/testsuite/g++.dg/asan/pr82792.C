/* PR sanitizer/82792 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=address" } */

extern int
test (int i, int j)
{
  long c;
  (c) = 1;
  switch (i)
    {
    case 1:
      if (j)
	{
	  c = 1;
	}
      goto default_case;
    case 2:
      {
	if (j)
	  {
	    c = 0;
	  }
      }
      __attribute ((fallthrough));
    default_case:
    default:
      c = 0;
      break;
    }
  return 0;
}
