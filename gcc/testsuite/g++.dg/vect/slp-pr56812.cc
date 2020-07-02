/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_hw_misalign } */
/* { dg-additional-options "-O3 -funroll-loops -fvect-cost-model=dynamic -fopt-info-vec" } */

class mydata {
public:
    mydata() {Set(-1.0);}
    void Set (float);
    static int upper() {return 8;}
    float data[8];
};

void mydata::Set (float x)
{
  /* We want to vectorize this either as loop or basic-block.  */
  for (int i=0; i<upper(); i++) /* { dg-optimized "\[^\n\]* vectorized" "" { target *-*-* } 0 } */
    data[i] = x;
}
