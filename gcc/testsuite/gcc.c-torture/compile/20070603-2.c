typedef _Complex double ar[];
int f(ar *a, unsigned int n)
{
  unsigned int i;
  for(i = 0; i< n; i++)
    {
      (*a)[i*4] = __real__ (*a)[(i+1)*4] + __real__ (*a)[i*4];
    }
}
