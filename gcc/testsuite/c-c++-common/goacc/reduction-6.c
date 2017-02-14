/* Check if different occurences of the reduction clause on
   OpenACC loop nests will compile.  */

int foo (int N)
{
  int a = 0, b = 0, c = 0, d = 0, e = 0;

  #pragma acc parallel
  {
    #pragma acc loop
    for (int i = 0; i < N; i++)
      {
        #pragma acc loop reduction(+:a)
	for (int j = 0; j < N; j++)
	  a += 1;
      }
  }

  #pragma acc parallel
  {
    #pragma acc loop reduction(+:b)
    for (int i = 0; i < N; i++)
      {
        #pragma acc loop
	for (int j = 0; j < N; j++)
	  b += 1;
      }
  }

  #pragma acc parallel
  {
    #pragma acc loop reduction(+:c)
    for (int i = 0; i < N; i++)
      {
        #pragma acc loop reduction(+:c)
	for (int j = 0; j < N; j++)
	  c += 1;
      }
  }

  #pragma acc parallel loop
  for (int i = 0; i < N; i++)
    {
      #pragma acc loop reduction(+:d)
      for (int j = 0; j < N; j++)
	d += 1;
    }

  #pragma acc parallel loop reduction(+:e)
  for (int i = 0; i < N; i++)
    {
      #pragma acc loop reduction(+:e)
      for (int j = 0; j < N; j++)
	e += 1;
    }

  return a + b + c + d + e;
}
