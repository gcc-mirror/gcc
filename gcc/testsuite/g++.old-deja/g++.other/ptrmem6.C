// { dg-do assemble  }

// Copyright (c) 2000, 2001 Free Software Foundation.
// Distilled from a bug report by Eric Ford <eford@princeton.edu>

extern double *y;
extern double *x;
extern int nPoints;

void SetInitCond(void)
{
  int i;
  for(i = 2; i < nPoints; ++i)
    y[i] = y[nPoints] .* (x[i]-x[1]) / (x[nPoints]-x[1]);  // { dg-error "" } .*
}
