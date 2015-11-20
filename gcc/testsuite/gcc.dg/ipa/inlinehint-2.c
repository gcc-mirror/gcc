/* { dg-options "-O3 -c -fdump-ipa-inline-details -fno-early-inlining -fno-ipa-cp"  } */
/* { dg-add-options bind_pic_locally } */

int
t(int s, void **p)
{
  int i;
  for (i=0;i<10000;i+=s)
    p[i]=0;
}
int
m(void **p)
{
  t (10, p);
}
/* { dg-final { scan-ipa-dump "loop_stride"  "inline"  } } */
