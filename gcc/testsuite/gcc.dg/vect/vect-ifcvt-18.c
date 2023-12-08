/* { dg-require-effective-target vect_condition } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-Ofast -mavx" { target avx_runtime } } */


int A0[4] = {36,39,42,45};
int B0[4] = {42,42,0,42};
float A1[8] = {36,39,42,45,43,32,21,12};
float B1[8] = {42,42,0,42,42,42,0,42};
double A2[16] = {36,39,42,45,43,32,21,12,23,34,45,56,42,78,89,11};
double B2[16] = {42,42,0,42,42,42,42,42,42,42,42,42,0,42,42,42};

int main ()
{
  int i, j;
  int res0 = 1;
  float res1 = 1.0;
  double res2 = 1.0;

  for (i = 0; i < 4; i++)
    if (B0[i])
      res0 *= A0[i];

  for (i = 0; i < 8; i++)
    if (B1[i])
      res1 *= A1[i];
  
  for (i = 0; i < 16; i++)
    if (B2[i])
      res2 *= A2[i];
  /* check results:  */
  if (res0 != 63180 || res1 != 1043228160.000000
      ||res2 != 3296728515318523101184.000000)
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 3 loops" "vect" { target i?86-*-* x86_64-*-* } } } */
