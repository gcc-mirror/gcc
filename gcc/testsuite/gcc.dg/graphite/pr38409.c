/* { dg-options "-O2" } */

typedef struct test input ;
struct test 
{
  int type ;
  int symflag ;
};
Chv_copyEntriesToVector ( input *chv,double *dvec) 
{
  double *entries ;
  int mm, nent;
  int first, i, k , stride ;
  if ( ((chv)->type == 1) ) 
    {
      for ( i = 0 ; i < 10 ; i++)
        {
	  dvec[2*mm] = entries[2*k] ;
	  k += stride ;
	  stride -= 2 ;
        }
    }
  return(mm) ; 
}
