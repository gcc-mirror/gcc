/* { dg-do compile } */

void func(double * __restrict__ v1, double * v2, unsigned n)
{
  for (unsigned i = 0; i < n; ++i)
    v1[i] = v2[i];
}

/* { dg-final { scan-tree-dump-not "Alignment of access forced using peeling" "vect" } } */
