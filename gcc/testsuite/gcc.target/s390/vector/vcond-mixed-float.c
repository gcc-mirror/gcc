/* Check for vectorization of mixed conditionals.  */
/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z14 -mzarch -ftree-vectorize -fdump-tree-vect-details" } */

float xf[1024];
float zf[1024];
float wf[1024];

int xi[1024];
int zi[1024];
int wi[1024];

void fooif ()
{
  int i;
  for (i = 0; i < 1024; ++i)
    zf[i] = xi[i] ? zf[i] : wf[i];
}

void foofi ()
{
  int i;
  for (i = 0; i < 1024; ++i)
    zi[i] = xf[i] ? zi[i] : wi[i];
}

void fooif2 ()
{
  int i;
  for (i = 0; i < 1024; ++i)
    zf[i] = (xf[i] > 0) ? zf[i] : wf[i];
}

void fooif3 ()
{
  int i;
  for (i = 0; i < 1024; ++i)
    zf[i] = (xf[i] > 0.f & wf[i] < 0.f) ? zf[i] : wf[i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" } } */
