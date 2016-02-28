/* { dg-do compile } */

extern int a, b, d;
extern char c[];
void
fn1 (void)
{
  for (;;)
    {
      if (b)
	{
LABEL_T5T5T:
	  for (; d < a; d++)
	    c[d] = 6;
	}
      break;
    }
  if (a > 6)
    {
      a = 4;
      goto LABEL_T5T5T;
    }
}
