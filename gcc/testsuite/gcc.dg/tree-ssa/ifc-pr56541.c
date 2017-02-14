/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ifcvt-stats-blocks-details" } */

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

/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* Sum is wrong here, but not enough for error to be reported.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming frequencies" 0 "ifcvt" } } */
