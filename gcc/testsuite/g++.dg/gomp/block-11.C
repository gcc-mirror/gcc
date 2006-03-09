/* PR c++/24516 */
/* { dg-do compile } */

void
bar (int *p)
{
  int m;
#pragma omp parallel for
  for (m = 0; m < 1000; ++m)
    switch (p[m])
      {
      case 1:
	p[m] = 2;
	break;
      default:
	p[m] = 3;
	break;
      }
}
