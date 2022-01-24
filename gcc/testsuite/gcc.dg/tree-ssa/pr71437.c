/* { dg-do compile } */
/* { dg-options "-ffast-math -O3 -fdump-tree-dom3-details" } */

int I = 50, J = 50;
int S, L;
const int *pL;
const int *pS;

void bar (float, float);

void foo (int K)
{
  int k, i, j;
  static float LD, SD;
  for (k = 0 ; k < K; k++)
    {
        for( i = 0 ; i < ( I - 1 ) ; i++ )
        {
            if( ( L < pL[i+1] ) && ( L >= pL[i] ) )
              break ;
        }

        if( i == ( I - 1 ) )
          L = pL[i] ;
        LD = (float)( L - pL[i] ) /
                        (float)( pL[i + 1] - pL[i] ) ;

        for( j = 0 ; j < ( J-1 ) ; j++ )
        {
            if( ( S < pS[j+1] ) && ( S >= pS[j] ) )
              break ;
        }

        if( j == ( J - 1 ) )
          S = pS[j] ;
        SD = (float)( S - pS[j] ) /
                         (float)( pS[j + 1] - pS[j] ) ;

	bar (LD, SD);
    }
}

/* We used to get 1 vrp-thread1 candidates here, but they now get
   deferred until after loop opts are done, because they were rotating
   loops.  */
/* { dg-final { scan-tree-dump-times "Threaded jump " 2 "dom3" } } */
