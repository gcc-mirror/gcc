/*  pr69193 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

int
main (void)
{
  int i = 0;
  int j = _Generic (i,
		    int: 0,
		    long int: (i = (long int) 9223372036854775808UL));
  return i + j;
}


