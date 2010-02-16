/* { dg-do compile } */

float
pvslockprocess(float *fout, float *fin, int framesize)
{
  int i;
  float mag=0.0f, diff;
  for (i = 0; i < framesize; i += 2) {
      mag += fin[i];
      fout[i] = fin[i];
      fout[i+1] = fin[i+1];
  }
  return mag;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
