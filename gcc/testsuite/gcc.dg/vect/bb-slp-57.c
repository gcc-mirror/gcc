/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-require-effective-target vect_float } */

float *a;
typedef struct {
  int c;
  float bbmax[3];
} d;
d e;
int f[3];
int g, h, i, j;
float k, k;
void l()
{
  for (unsigned z = 0; z < 2048; ++z) {
    {
      j = e.bbmax[1] > k ? e.bbmax[1] : k;
    }
    e.bbmax[1] = j;
    { i = e.bbmax[2] > k ? e.bbmax[2] : k; }
    e.bbmax[2] = i;
    f[2] = a[2];
    {
      float b;
      h = e.bbmax[1] > b ? e.bbmax[1] : b;
    }
    e.bbmax[1] = h;
    {
      float b;
      g = e.bbmax[2] > b ? e.bbmax[2] : b;
    }
    e.bbmax[2] = g;
  }
}

/* { dg-final { scan-tree-dump-times "transform load" 1 "slp1" { target { { x86_64-*-* i?86-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "optimized: basic block" "slp1" { target { { x86_64-*-* i?86-*-* } && lp64 } } } } */
