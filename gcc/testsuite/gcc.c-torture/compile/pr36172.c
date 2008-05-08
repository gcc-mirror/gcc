int f(float * );
unsigned long FcCharSetFreeze (int *fcs, int b)
{
  int i;
  int a = 0;
  for (i = 0; i < *fcs; i++)
  {
    float *leaf = (float *)fcs;
    int hash = f (leaf);
    if (hash)
      a = b;
    if (!a)
      return;
  }
  return (unsigned long) fcs;
}

