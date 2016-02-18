/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-Ofast -funroll-loops" } */

#define NXX 516
#define NYY 516
#define IND(x,y) ((x) + (y)*NXX)
float **In, **Out, **V;

void foo(int I, int J, int K1, int K2, int L1, int L2 )
{
  for(int i=0; i < I; i++)
    {
      float *v = V[i];

      for(int j=0; j < J; j++)
	{
	  float *in = In[j];
	  float *out = Out[j];
	  for(int l=L1; l<L2; l++)
	    {
	      for(int k=K1; k<K2; k++)
		{
		  float sum = 0;
		  int offset = 0;
		  for(int m=-2; m<=2; m++)
		    {
		      for(int n=-2; n<=2; n++, offset++)
			sum += in[IND((k+n), (l+m))] * v[offset];
		    }
		  out[IND(k,l)] = sum;
		}
	    }

	}
    }
}

/* { dg-final { scan-tree-dump "improved number of alias checks from \[0-9\]* to 2" "vect" } } */
