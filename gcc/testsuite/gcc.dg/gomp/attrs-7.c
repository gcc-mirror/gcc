/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c23" } */

void
foo ()
{

  [[omp::directive (parallel sections)]]
  {
    [[omp::directive (parallel)]];
    [[omp::sequence (directive (section), directive (flush))]];		/* { dg-error "must be the only specified attribute on a statement" } */
									/* { dg-error "expected '#pragma omp section' or '\\\}'" "" { target *-*-* } .-2 } */
    [[omp::sequence (directive (flush), omp::directive (section))]];	/* { dg-error "must be the only specified attribute on a statement" } */
    [[gnu::cold, omp::directive (section)]];				/* { dg-error "must be the only specified attribute on a statement" } */
    [[omp::directive (section)]] [[gnu::cold]];				/* { dg-error "must be the only specified attribute on a statement" } */
    [[omp::directive (section foo)]];					/* { dg-error "expected end of line before 'foo'" } */
  }
}

int
bar (int a, int *c, int *d, int *e, int *f)
{
  int i;
  [[omp::directive (parallel for reduction (inscan, +: a))]]				/* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      [[omp::sequence (omp::directive (parallel), omp::directive (scan, exclusive (a)))]]	/* { dg-error "must be the only specified attribute on a statement" } */
      a += c[i];
    }											/* { dg-error "expected" } */
  [[omp::directive (parallel for reduction (inscan, +: a))]]				/* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      [[omp::sequence (directive (scan inclusive (a)), directive (critical))]]		/* { dg-error "must be the only specified attribute on a statement" } */

      d[i] = a;
    }											/* { dg-error "expected" } */
  [[omp::directive (parallel for reduction (inscan, +: a))]]				/* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      [[gnu::cold]] [[omp::directive (scan, exclusive (a))]]
											/* { dg-error "#pragma omp scan" "" { target *-*-* } .-1 } */
      a += c[i];									/* { dg-warning "'cold' attribute ignored" } */
    }											/* { dg-error "expected" } */
  [[omp::directive (parallel for reduction (inscan, +: a))]]				/* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      [[omp::directive (scan, exclusive (a)), gnu::cold]]				/* { dg-error "must be the only specified attribute on a statement" } */
      a += c[i];
    }											/* { dg-error "expected" } */
  [[omp::directive (parallel for reduction (inscan, +: a))]]				/* { dg-error "'a' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      [[omp::directive (scan)]]								/* { dg-error "expected 'inclusive' or 'exclusive' clause before end of line" } */
      a += c[i];
    }
  return a;
}
