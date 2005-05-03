/* { dg-do compile } */
/* { dg-options "-O1" } */

extern double floor (double);

long foo (float f)
{
	  return (long) floor (f);
}
