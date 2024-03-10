/* { dg-do compile } */

/* Braces may enclose a nested FOR even when intervening code is not
   permitted.  Before GCC implemented OpenMP 5.1 canonical loop syntax
   and support for intervening code, it used to ignore empty statements
   instead of treating them as intervening code; as an extension, those
   are still accepted without complaint even in constructs where intervening
   code is not supposed to be valid.  */

void s1 (int a1, int a2, int a3)
{
  int i, j, k;

#pragma omp for ordered(3)
  for (i = 0; i < a1; i++)
    {
      for (j = 0; j < a2; j++)
	{
	  for (k = 0; k < a3; k++)
	    {
	    }
	}
    }
}

void s2 (int a1, int a2, int a3)
{
  int i, j, k;

#pragma omp for ordered(3)
  for (i = 0; i < a1; i++)
    {
      ;
      for (j = 0; j < a2; j++)
	{
	  ;
	  for (k = 0; k < a3; k++)
	    {
	    }
	  ;
	}
      ;
    }
}
