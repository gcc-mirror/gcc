/* { dg-options "-O3 -c -fdump-ipa-inline-details -fno-early-inlining -fno-ipa-cp"  } */
t(int s, void **p)
{
  int i;
  for (i;i<10000;i+=s)
    p[i]=0;
}
m(void **p)
{
  t (10, p);
}
/* { dg-final { scan-ipa-dump "loop_stride"  "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
