void f1 (int, int, int);
int iii (int, int, int);

int
s1 (int a1, int a2, int a3)
{
  int r = 0;
  #pragma omp simd collapse(3) reduction (inscan, +:r)
  for (int i = 0; i < a1; i++)
    for (int j = 0; j < a2; j++)
      for (int k = 0; k < a3; k++)
	{
	  #pragma omp scan exclusive (r)  /* { dg-warning "'#pragma omp scan' with zero preceding executable statements" } */
	  f1 (2, k, r);
	}

  #pragma omp simd collapse(3) reduction (inscan, +:r)
  for (int i = 0; i < a1; i++)
    for (int j = 0; j < a2; j++)
      for (int k = 0; k < a3; k++)
	{
	  r += iii (i, j, k);
	  #pragma omp scan exclusive (r)  /* { dg-warning "'#pragma omp scan' with zero succeeding executable statements" } */
	}

  #pragma omp simd collapse(3) reduction (inscan, +:r)
  for (int i = 0; i < a1; i++)
    for (int j = 0; j < a2; j++)
      for (int k = 0; k < a3; k++)
	{
	  #pragma omp scan inclusive (r)
           /* { dg-warning "'#pragma omp scan' with zero preceding executable statements" "" { target *-*-* } .-1 } */
           /* { dg-warning "'#pragma omp scan' with zero succeeding executable statements" "" { target *-*-* } .-2 } */
	}
  return r;
}

int
s2 (int a1, int a2, int a3)
{
  int r = 0;
  #pragma omp simd collapse(3) reduction (inscan, +:r)  /* { dg-error "'r' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (int i = 0; i < a1; i++)
    for (int j = 0; j < a2; j++)
      for (int k = 0; k < a3; k++)
	{
	  f1 (2, k, r);
	  r += iii (i, j, k);  /* { dg-error "expected '#pragma omp scan'" "" { target c++ } } */
	}  /* { dg-error "expected '#pragma omp scan'" "" { target c } } */

  r = 0;
  #pragma omp simd collapse(3) reduction (inscan, +:r)  /* { dg-error "'r' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (int i = 0; i < a1; i++)
    for (int j = 0; j < a2; j++)
      for (int k = 0; k < a3; k++)
	;  /* { dg-error "expected '\{' before ';' token" } */

  #pragma omp simd collapse(3) reduction (inscan, +:r)  /* { dg-error "'r' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (int i = 0; i < a1; i++)
    for (int j = 0; j < a2; j++)
      for (int k = 0; k < a3; k++)
	{
	}  /* { dg-error "expected expression before '\}' token" "" { target c } } */
           /* { dg-error "expected primary-expression before '\}' token" "" { target c++ } .-1 } */
	   /* { dg-error "expected '#pragma omp scan'" "" { target *-*-* } .-2 } */


  r = 0;
  #pragma omp simd collapse(3) reduction (inscan, +:r)
  for (int i = 0; i < a1; i++)
    for (int j = 0; j < a2; j++)
      for (int k = 0; k < a3; k++)
	{
	  f1 (2, k, r);
	  #pragma omp scan inclusive (r)
	  #pragma omp scan inclusive (r)  /* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */
	  r += iii (i, j, k);
	}

  #pragma omp scan inclusive (r)  /* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */

  r = 0;
  #pragma omp simd collapse(3) reduction (inscan, +:r)  /* { dg-error "'r' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" } */
  for (int i = 0; i < a1; i++)
    for (int j = 0; j < a2; j++)
      for (int k = 0; k < a3; k++)
	{
	  f1 (2, k, r);
	  {
	    #pragma omp scan inclusive (r)  /* { dg-error "'#pragma omp scan' may only be used in a loop construct with 'inscan' 'reduction' clause" } */
	  }
	  r += iii (i, j, k);  /* { dg-error "expected '#pragma omp scan'" "" { target c++ } }  */
	}  /* { dg-error "expected '#pragma omp scan'" "" { target c } }  */
  return r;
}
