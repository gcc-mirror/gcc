/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_hw_misalign } */
/* { dg-additional-options "-O3 -funroll-loops -fvect-cost-model=dynamic" } */

class mydata {
public:
    mydata() {Set(-1.0);}
    void Set (float);
    static int upper() {return 8;}
    float data[8];
};

void mydata::Set (float x)
{
  for (int i=0; i<upper(); i++)
    data[i] = x;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized" 1 "slp1" } } */
