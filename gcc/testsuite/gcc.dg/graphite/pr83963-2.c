/* { dg-do compile } */
/* { dg-options "-O -floop-nest-optimize" } */

int Chv_countBigEntries (int npivot, int pivotsizes[], int countflag,
			 double droptol, int nD)
{
  double absval ;
  double *entries ;
  int count;
  int ii, jj, kinc, kk, kstart, stride ;
  for ( ii = 0 ; ii < nD ; ii++ )
    { 
      kk = kstart ;  
      kinc = stride ;
      for ( jj = 0 ; jj < ii ; jj++ )
	{
	  absval = __builtin_fabs(entries[kk]) ; 
	  if ( absval >= droptol )
	    count++ ;   
	  kk += kinc ;    
	  kinc -= 2 ; 
	}
      kstart-- ;   
    }
  return count;
}
