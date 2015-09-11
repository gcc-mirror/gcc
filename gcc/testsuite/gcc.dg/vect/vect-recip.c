/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

void f(float *__restrict__ qa, float *__restrict__ qb,
       float *__restrict__ qc, float *__restrict__ rtrms)
{
  int i;
  static float qam[600];
  static float qbm[600];
  static float qcm[600];
  for(i=0;i<600;i++)
  {
    float a = rtrms[i];
    qam[i] = qa[i]/a;
    qbm[i] = qb[i]/a;
    qcm[i] = qc[i]/a;
  }
}

