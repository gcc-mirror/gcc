/* Check for vectorization of mixed conditionals.  */
/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z14 -mzarch -ftree-vectorize -fdump-tree-vect-details" } */

double xd[1024];
double zd[1024];
double wd[1024];

long xl[1024];
long zl[1024];
long wl[1024];

void foold ()
{
  int i;
  for (i = 0; i < 1024; ++i)
    zd[i] = xl[i] ? zd[i] : wd[i];
}

void foodl ()
{
  int i;
  for (i = 0; i < 1024; ++i)
    zl[i] = xd[i] ? zl[i] : wl[i];
}

void foold2 ()
{
  int i;
  for (i = 0; i < 1024; ++i)
    zd[i] = (xd[i] > 0) ? zd[i] : wd[i];
}

void foold3 ()
{
  int i;
  for (i = 0; i < 1024; ++i)
    zd[i] = (xd[i] > 0. & wd[i] < 0.) ? zd[i] : wd[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" } } */
