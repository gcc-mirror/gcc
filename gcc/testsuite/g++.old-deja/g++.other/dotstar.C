// Build don't link:

extern double *y;
extern double *x;
extern int nPoints;

void SetInitCond(void)
{
  int i;
  for(i = 2; i < nPoints; ++i)
    y[i] = y[nPoints] .* (x[i]-x[1]) / (x[nPoints]-x[1]);
}
