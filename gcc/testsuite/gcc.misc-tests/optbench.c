/* ----------------------------------------------------- * 
 |                                                       | 
 |    PC Tech Journal Benchmark Series                   | 
 |    C Code Optimization Benchmark                      | 
 |                                                       | 
 |    Copyright (c) 1988 Ziff-Davis Publishing Company   | 
 |                                                       | 
 |    This benchmark code is designed to test the        | 
 |    code optimization techniques applied by a C        | 
 |    compiler.  It does not produce meaningful results  | 
 |    when executed, or represent good style.            | 
 |                                                       | 
 * ----------------------------------------------------- */ 
 
#include <string.h> 

#define max_vector   2 
#define constant5    5 
 
typedef    unsigned char   uchar; 
 
int    i,  j,  k , l,  m; 
int    i2, j2, k2; 
int    g3, h3, i3, k3, m3; 
int    i4, j4; 
int    i5, j5, k5; 
 
double flt_1, flt_2, flt_3, flt_4, flt_5, flt_6; 
 
int    ivector[ 3 ]; 
uchar  ivector2[ 3 ]; 
short  ivector4[ 6 ]; 
int    ivector5[ 203 ]; 
 
#ifndef NO_PROTOTYPES 
void   dead_code( int, char * ); 
void   unnecessary_loop( void ); 
void   loop_jamming( int ); 
void   loop_unrolling( int ); 
int    jump_compression ( int, int, int, int, int ); 
#else 
void   dead_code(); 
void   unnecessary_loop(); 
void   loop_jamming(); 
void   loop_unrolling(); 
int    jump_compression(); 
#endif 
 
 
int     main( argc, argv )      /* optbench */ 
        int  argc; 
        char     **argv; 
        { 
        /* ------------------------------ * 
         | Constant and copy propagation  | 
         * ------------------------------ */ 
 
        j4 = 2; 
        if( i2 < j4 && i4 < j4 ) 
           i5 = 2; 
 
        j4 = k5; 
        if( i2 < j4 && i4 < j4 ) 
           i5 = 3; 
        
        /* ---------------------------------------- * 
         |  Constant folding, arithmetic identities | 
         |  and redundant load/store operations     | 
         * ---------------------------------------- */ 
 
        i3 = 1 + 2; 
        flt_1 = 2.4 + 6.3; 
        i2 = 5;               
        j2 = i + 0; 
        k2 = i / 1; 
        i4 = i * 1; 
        i5 = i * 0; 
 
#ifndef NO_ZERO_DIVIDE 
        /* 
         *   Some compilers correctly recognize a zero divide 
         *   error and do not generate any object code. 
         */ 
/*
        i2 = i / 0;            
        flt_2 = flt_1 / 0.0; 
*/
#else    
/*
        printf( "This compiler handles divide-by-zero as an
error\n"); 
*/;
#endif 
        flt_3 = 2.4 / 1.0;  
        flt_4 = 1.0 + 0.0000001; 
        flt_5 = flt_6 * 0.0; 
        flt_6 = flt_2 * flt_3; 
  
        /* ------------ * 
         |  Dead store  | 
         * ------------ */ 
 
         k3 = 1; 
         k3 = 1; 
 
        /* --------------------- * 
         |  Strength reduction   | 
         * --------------------- */ 
 
        k2 = 4 * j5; 
           for( i = 0; i <= 5; i++ )
              ivector4[ i ] = i * 2; 
    
        /* ----------- * 
         | Simple loop | 
         * ----------- */ 
 
        j5 = 0; 
        k5 = 10000; 
        do { 
              k5 = k5 - 1; 
              j5 = j5 + 1; 
              i5 = ( k5 * 3 ) / ( j5 * constant5 ); 
           } while ( k5 > 0 ); 
 
        /* ----------------------------------- * 
         | Loop induction variable handling    | 
         * ----------------------------------- */ 
 
        for( i = 0; i < 100; i++) 
           ivector5[ i * 2 + 3 ] = 5; 
 
        /* ----------------------------- * 
         | Very busy expression handling | 
         * ----------------------------- */ 
 
        if( i < 10 ) 
           j5 = i5 + i2; 
        else 
           k5 = i5 + i2; 
 
        /* -------------------------------------------- * 
         | Check how the compiler generates the address | 
         | of a variable with a constant subscript,     | 
         | copy propagation, and register propagation.  | 
         * -------------------------------------------- */ 
 
         ivector[ 0  ] = 1;  /* constant address generation */ 
         ivector[ i2 ] = 2;  /* i2 should be a propagated value
*/ 
         ivector[ i2 ] = 2;  /* register propagation */ 
         ivector[ 2  ] = 3;  /* constant address generation */ 
     
     
         /* ---------------------------------- * 
          | Common subexpression elimination   | 
          * ---------------------------------- */ 
  
        if(( h3 + k3 ) < 0 || ( h3 + k3 ) > 5 )
        /* 
               printf("Common subexpression elimination\n"); 
        
        */;
        else { 
               m3 = ( h3 + k3 ) / i3; 
               g3 = i3 + (h3 + k3); 
         } 
        /* --------------------------------------- * 
         | Invariant code motion                   | 
         |  (j * k) can be moved outside the loop. |  
         * --------------------------------------- */ 
  
        for( i4 = 0; i4 <= max_vector; i4++ )    
           ivector2[ i4 ] = j * k; 
  
        /* ---------------------------- * 
         | Function call with arguments | 
         * ---------------------------- */ 
 
        dead_code( 1, "This line should not be printed" ); 
 
        /* ------------------------------- * 
         | Function call without arguments | 
         * ------------------------------- */ 
 
        unnecessary_loop(); 

	exit (0);
    
        }  /* end of main */ 
 
            
/* --------------------------------------------- * 
 | Function: dead_code                           | 
 |           Test for dead code and dead stores. | 
 |           NO code should be generated.        | 
 * ----------------------------------------------*/ 
 
void   dead_code( a, b ) 
       int a; 
       char *b; 
       { 
         int idead_store; 
 
         idead_store = a; 
         if( 0 )
         /* 
           printf( "%s\n", b );
          */; 
       }  /* end of dead_code */ 
 
/* ----------------------------------------------- * 
 | Function: unnecessary_loop                      | 
 |           The loop in the following function is | 
 |           not necessary since the value of the  | 
 |           assignment is constant. Ideally, the  | 
 |           loop should be optimized out.         | 
 * ----------------------------------------------- */ 
 
void   unnecessary_loop() 
       { 
           int x;  
 
           x = 0; 
           for( i = 0; i < 5; i++ ) /* loop should not be generated */ 
           k5 = x + j5;                  
       }   /* end of unnecessary_loop */ 
 
/* --------------------------------------------- * 
 | Function: loop_jamming                        | 
 |           The two loop in this function share | 
 |           the same loop conditions and could  | 
 |           be coalesced together.              | 
 * --------------------------------------------- */ 
 
void   loop_jamming( x ) 
       int x; 
       { 
          for( i = 0; i < 5; i++ ) 
             k5 = x + j5 * i; 
          for( i = 0; i < 5; i++ ) 
             i5 = x * k5 * i; 
       }  /* end of loop_jamming */ 
 
/* ------------------------------------------------ * 
 | Function: loop_unrolling                         | 
 |           The loop in this function should be    | 
 |           replaced with three inline word stores | 
 |           using constant array arithmetic or by  | 
 |           specialized machine instructions used  | 
 |           for block memory initializiation.      | 
 * ------------------------------------------------ */ 
 
void  loop_unrolling( x ) 
      int x; 
      { 
         for( i = 0; i < 6; i++ ) 
           ivector4[ i ] = 0; 
       }  /* end of loop_unrolling */ 
 
/* ------------------------------------------------------ * 
 | Function: jump_compression                             | 
 |           This awkward code is useful to illustrate    | 
 |           jump chain compression. The goto 'end_1' can | 
 |           be replaced by a direct jump to 'beg_1'.     | 
 *------------------------------------------------------ */ 
 
int  jump_compression ( i, j, k, l, m ) 
int i,j ,l, m; 
        { 
beg_1: 
           if( i < j ) 
               if( j < k ) 
                   if( k < l ) 
                       if ( l < m ) 
                           l += m; 
                       else 
                           goto end_1;
                   else 
                       k += l; 
               else { 
                   j += k; 
end_1:            goto beg_1; 
               } 
           else 
               i += j; 
          return( i + j + k + l + m ); 
        }   /* end of jump_compression */ 
