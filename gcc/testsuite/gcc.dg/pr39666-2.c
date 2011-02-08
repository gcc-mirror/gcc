/* PR middle-end/39666 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int
foo (int i)
{
  int j;
  switch (i)
    {
    case -__INT_MAX__ - 1 ... -1:
      j = 6;
      break;
    case 0:
      j = 5;
      break;
    case 2 ... __INT_MAX__:
      j = 4;
      break;
    }
  return j;	/* { dg-warning "may be used uninitialized" } */
}
