
int f(_Complex double *a, unsigned int n)
{
  unsigned int i;
  for(i = 0; i< n; i++)
    {
      a[i] = __real__ a[i+1] + __real__ a[i];
    }
}
