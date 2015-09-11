/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_int_mult } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

void train(short *t, short *w, int n, int err)
{
  n=(n+7)&-8;
  for (int i=0; i<n; ++i)
    {
      int wt=w[i]+((t[i]*err*2>>16)+1>>1);
      if (wt<-32768) wt=-32768;
      if (wt>32767) wt=32767;
      w[i]=wt;
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail vect_no_int_min_max } } } */
