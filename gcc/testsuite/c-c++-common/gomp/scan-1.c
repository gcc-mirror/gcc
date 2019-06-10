int a, b;

void
f1 (void)
{
  #pragma omp scan inclusive (a)	/* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */
  #pragma omp scan exclusive (b)	/* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */
}

void
f2 (int *c, int *d, int *e, int *f)
{
  int i, l = 1;
  #pragma omp for reduction (inscan, +: a) reduction (+: b)	/* { dg-error "'inscan' and non-'inscan' 'reduction' clauses on the same construct" } */
  for (i = 0; i < 64; i++)
    {
      { b++; a += c[i]; }
      #pragma omp scan inclusive (a)				/* { dg-error "" } */
      d[i] = a;
    }
  #pragma omp for reduction (+: a) reduction (inscan, +: b)	/* { dg-error "'inscan' and non-'inscan' 'reduction' clauses on the same construct" } */
  for (i = 0; i < 64; i++)
    {
      { a++; b += c[i]; }
      #pragma omp scan inclusive (b)				/* { dg-error "" } */
      d[i] = b;
    }
  #pragma omp for reduction (inscan, +: e[:2])			/* { dg-error "'inscan' 'reduction' clause with array section" } */
  for (i = 0; i < 64; ++i)
    {
      { e[0] += c[i]; e[1] += c[i]; }
      #pragma omp scan inclusive (a, e[:2])			/* { dg-error "" } */
      { d[i] = e[0]; f[i] = e[1]; }
    }
  #pragma omp for reduction (inscan, +: a) ordered		/* { dg-error "'ordered' clause specified together with 'inscan' 'reduction' clause" } */
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      #pragma omp scan inclusive (a)				/* { dg-error "" } */
      d[i] = a;
    }
  #pragma omp for reduction (inscan, +: a) ordered(1)		/* { dg-error "'ordered' clause specified together with 'inscan' 'reduction' clause" } */
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      #pragma omp scan inclusive (a)				/* { dg-error "" } */
      d[i] = a;
    }
  #pragma omp for reduction (inscan, +: a) schedule(static)	/* { dg-error "'schedule' clause specified together with 'inscan' 'reduction' clause" } */
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      #pragma omp scan inclusive (a)				/* { dg-error "" } */
      d[i] = a;
    }
  #pragma omp for reduction (inscan, +: a) schedule(static, 2)	/* { dg-error "'schedule' clause specified together with 'inscan' 'reduction' clause" } */
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      #pragma omp scan inclusive (a)				/* { dg-error "" } */
      d[i] = a;
    }
  #pragma omp for reduction (inscan, +: a) schedule(nonmonotonic: dynamic, 2)	/* { dg-error "'schedule' clause specified together with 'inscan' 'reduction' clause" } */
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      #pragma omp scan inclusive (a)				/* { dg-error "" } */
      d[i] = a;
    }
  #pragma omp for reduction (inscan, +: a) linear (l)		/* { dg-error "'inscan' 'reduction' clause used together with 'linear' clause for a variable other than loop iterator" } */
  for (i = 0; i < 64; i++)
    {
      { a += c[i]; ++l; }
      #pragma omp scan inclusive (a)
      d[i] = a;
    }
}

void
f3 (int *c, int *d)
{
  int i;
  #pragma omp teams reduction (inscan, +: a)	/* { dg-error "'inscan' 'reduction' clause on 'teams' construct" } */
  ;
  #pragma omp parallel reduction (inscan, +: a)	/* { dg-error "'inscan' 'reduction' clause on 'parallel' construct" } */
  ;
  #pragma omp sections reduction (inscan, +: a)	/* { dg-error "'inscan' 'reduction' clause on 'sections' construct" } */
  {
    #pragma omp section
    ;
  }
  #pragma omp target parallel for reduction (inscan, +: a) map (c[:64], d[:64])	/* { dg-error "'inscan' 'reduction' clause on construct other than 'for', 'simd', 'for simd', 'parallel for', 'parallel for simd'" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan exclusive (a)	/* { dg-error "" } */
      a += c[i];
    }
  #pragma omp teams
  {
  #pragma omp distribute parallel for reduction (inscan, +: a)	/* { dg-error "'inscan' 'reduction' clause on construct other than 'for', 'simd', 'for simd', 'parallel for', 'parallel for simd'" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan exclusive (a)	/* { dg-error "" } */
      a += c[i];
    }
  #pragma omp distribute parallel for simd reduction (inscan, +: a)	/* { dg-error "'inscan' 'reduction' clause on construct other than 'for', 'simd', 'for simd', 'parallel for', 'parallel for simd'" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan exclusive (a)	/* { dg-error "" } */
      a += c[i];
    }
  }
}

void
f4 (int *c, int *d)
{
  int i;
  #pragma omp taskloop reduction (inscan, +: a)	/* { dg-error "'inscan' 'reduction' clause on 'taskloop' construct" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan exclusive (a)	/* { dg-error "" } */
      a += c[i];
    }
}

void
f5 (int *c, int *d)
{
  int i;
  #pragma omp simd reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan exclusive (a, b)	/* { dg-error "'b' specified in 'exclusive' clause but not in 'inscan' 'reduction' clause on the containing construct" } */
      a += c[i];
    }
}

void
f6 (int *c, int *d)
{
  int i;
  #pragma omp simd reduction (inscan, +: a, b)	/* { dg-error "'b' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan exclusive (a)
      a += c[i];
    }
}

void
f7 (void)
{
  int i;
  #pragma omp simd reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      if (i == 23)	/* { dg-error "invalid exit from OpenMP structured block" "" { target c++ } .+1 } */
	continue;	/* { dg-error "invalid branch to/from OpenMP structured block" "" { target c } } */
      else if (i == 27)
	goto l1;	/* { dg-error "invalid branch to/from OpenMP structured block" } */
      #pragma omp scan exclusive (a)
      {
	l1: a = 0;	/* { dg-error "jump to label 'l1'" "" { target c++ } } */
	if (i == 33)	/* { dg-error "invalid exit from OpenMP structured block" "" { target c++ } .+1 } */
	  continue;	/* { dg-error "invalid branch to/from OpenMP structured block" "" { target c } } */
      }
  }
}

void
f8 (int *c, int *d, int *e, int *f)
{
  int i;
  #pragma omp for reduction (inscan, +: a, b)		/* { dg-error "'b' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (i = 0; i < 64; i++)
    {
      { a += c[i]; b += d[i]; }
      #pragma omp scan inclusive (a) inclusive (b)	/* { dg-error "expected end of line before 'inclusive'" } */
      { e[i] = a; f[i] = b; }
    }
  #pragma omp for reduction (inscan, +: a, b)		/* { dg-error "'.' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (i = 0; i < 64; i++)
    {
      { a += c[i]; b += d[i]; }
      #pragma omp scan					/* { dg-error "expected 'inclusive' or 'exclusive' clause before end of line" } */
      { e[i] = a; f[i] = b; }
    }
}

void
f9 (void)
{
  int i;
  #pragma omp simd reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      if (i == 23)	/* { dg-error "invalid exit from OpenMP structured block" "" { target c++ } .+1 } */
	break;		/* { dg-error "break statement used with OpenMP for loop" "" { target c } } */
      #pragma omp scan exclusive (a)
      a++;
    }
}
