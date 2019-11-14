/* { dg-do compile } */
/* { dg-options "-O3 -mavx -mtune=generic -dp" } */
/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */

void feat_s3_cep_dcep (int cepsize_used, float **mfc, float **feat)
{
  float *f;
  float *w, *_w;
  int i;
  __builtin_memcpy (feat[0], mfc[0], cepsize_used * sizeof(float));
  f = feat[0] + cepsize_used;
  w = mfc[2];
  _w = mfc[-2];
  for (i = 0; i < cepsize_used; i++)
    f[i] = w[i] - _w[i];
}

/* { dg-final { scan-assembler-times "avx_vzeroupper" 2 } } */
