/* { dg-do compile { target { { i?86-*-* x86_64-*-* s390*-*-* } && lp64 } } } */
/* { dg-options "-O2 -fdump-tree-vrp2-details" } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to" 1 "vrp2" } } */

void v1 (unsigned long *in, unsigned long *out, unsigned int n)
{
  int i;

  for (i = 0; i < n; i++)
  {
    out[i] = in[i];
  }
}

void v2 (unsigned long *in, unsigned long *out, int n)
{
  int i;

  for (i = 0; i < n; i++)
  {
    out[i] = in[i];
  }
}

void v3 (unsigned long *in, unsigned long *out, unsigned int n)
{
  unsigned int i;

  for (i = 0; i < n; i++)
  {
    out[i] = in[i];
  }
}

void v4 (unsigned long *in, unsigned long *out, int n)
{
  unsigned int i;

  for (i = 0; i < n; i++)
  {
    out[i] = in[i];
  }
}
