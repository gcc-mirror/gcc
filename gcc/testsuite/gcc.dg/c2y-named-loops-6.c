/* N3355 - Named loops.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wall" } */

void
foo (int x)
{
 lab0:
  switch (x)
    {
    case 1:
      ++x;
      /* FALLTHRU */
    lab1:
    case 2:
      /* FALLTHRU */
    case 3:
    lab2:
      for (int i = 0; i < 4; ++i)
	if (i == 0)
	  continue lab2;
	else if (i == 1)
	  continue lab1;
	else if (x == 2)
	  break lab1;
	else
	  break lab0;
      break;
    default:
      break;
    }
}
