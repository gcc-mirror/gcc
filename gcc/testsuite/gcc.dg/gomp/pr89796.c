/* PR c++/89796 */
/* { dg-do compile } */
/* { dg-additional-options "-Wunused-value" } */

int
f1 (int *p)
{
  int r;
  #pragma omp atomic capture		/* { dg-bogus "value computed is not used" } */
  { r = *p; (*p)++; }
  return r;
}

int
f2 (int *p)
{
  int s
    = ({ int r;
	 #pragma omp atomic capture	/* { dg-bogus "value computed is not used" } */
	 { r = *p; (*p)++; }
	 r; });
  return s;
}
