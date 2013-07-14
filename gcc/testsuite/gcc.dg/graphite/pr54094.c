/* { dg-options "-O2 -floop-parallelize-all -floop-nest-optimize" } */
void dwt_deinterleave_h(int *a, int *b, int dn, int sn, int cas)
{
  int i;
  for (i=0; i<sn; i++)
    b[i]=a[2*i+cas];
  for (i=0; i<dn; i++)
    b[sn+i]=a[(2*i+1-cas)];
}

