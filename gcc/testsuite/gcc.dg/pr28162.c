/* { dg-do compile } */
/* { dg-options "-O3" } */

void Lag_max_wght(float corr[], long wght_flg)
{
     float t0, max;
     const float *ww;
     long i;
     if ( wght_flg > 0 ) {
        for ( i = 143; i >= 20; i-- ) {
           t0 = corr[ - i] * *ww--;
           if ( t0 >= max )
             max = t0;
        }
     }
}

