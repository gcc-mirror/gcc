/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-vect-details" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

float summul(int n, float *arg1, float *arg2)
{                                                  
    int i;                                             
    float res1 = 1.0;
    for(i = 0; i < n; i++) {
      if(arg2[i]) 
        res1 *= arg1[i];
    }                                                  
    return res1;                                       
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_masked_load } } } */
