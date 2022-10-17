#pragma omp nothing

struct S
{
  #pragma omp nothing
  int s;
};

int
foo (int i)
{
  #pragma omp nothing
  if (0)
    #pragma omp nothing
    i++;
  if (1)
    ;
  else
    #pragma omp nothing
    i++;
  switch (0)
    #pragma omp nothing
    {
    default:
      break;
    }
  while (0)
    #pragma omp nothing
    i++;
  for (; 0;)
    #pragma omp nothing
    i++;
  lab:
  #pragma omp nothing
  i++;
  return i;
}
