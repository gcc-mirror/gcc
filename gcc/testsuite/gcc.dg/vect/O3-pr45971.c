/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void
foo (int *x, int *y)
{
  int i;
  for (i = 0; i < 11; i++)
    y[i] = (x[i] == 1) ? i + 1 : -(i + 1);
}


