/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ifcvt-stats" } */

float a,b,c,d;

float z[1024]; int ok[1024];
const float rBig = 150.;

void foo()
{
  int i;

  for (i=0; i!=1024; ++i)
    {
      float rR = a*z[i];
      float rL = b*z[i];
      float rMin = (rR<rL) ? rR : rL;
      float rMax = (rR<rL) ? rL : rR;
      rMin = (rMax>0) ? rMin : rBig;
      rMin = (rMin>0) ? rMin : rMax;
      ok[i] = rMin-c<rMax+d;
    }
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
