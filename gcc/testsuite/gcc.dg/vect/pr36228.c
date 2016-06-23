/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

#define COLS         8
#define ROWS         8

int
t_run_test(void);

int
t_run_test()
{
     int k_1,i_1, j_1;
     static signed char f_1[ROWS][COLS] ;
     static long F_1[ROWS][COLS] ;
     long cosMatrixA[ROWS][COLS] ;

     for( k_1 = 0 ; k_1 < COLS ; k_1++ )
        {
            for( i_1 = 0 ; i_1 < ROWS ; i_1++ )
            {
                for( j_1 = 0 ; j_1 < COLS ; j_1++ )
                    F_1[i_1][j_1] += f_1[i_1][k_1] * cosMatrixA[k_1][j_1] ;
            }
        }

  return 0;
}

/* { dg-final { scan-tree-dump-times "versioning for alias required" 0 "vect" } } */
