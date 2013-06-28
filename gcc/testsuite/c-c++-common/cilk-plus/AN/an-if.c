/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#if HAVE_IO
#include <stdio.h>
#endif
#include <assert.h>

const int n = 8;

float x[8], y[8], z[8];

int main() {
    int i = 0;
    float x_sum =0;
    for(i=1; i<=5; i+=4 ) {
        x[0:n] = 3;
        y[0:n] = i;
        z[0:n] = 0;
#if HAVE_IO
        printf("x\ty\tz\n");
        for( size_t k=0; k<n; ++k ) {
            printf("%g\t%g\t%g\n",x[k],y[k],z[k]);
        }
	x_sum = __sec_reduce_add (x[0:n]);
	printf("sec_reduce_add (x[0:n]) = %6.3f\n", x_sum);
#endif
        assert( __sec_reduce_add(x[0:n])==3*n );
        assert( __sec_reduce_add(y[0:n])==i*n );
        assert( __sec_reduce_add(z[0:n])==0 );

        if (x[0:n] >= y[0:n]) {  
            z[0:n] = x[0:n] - y[0:n];
        } else {   
            z[0:n] = x[0:n] + y[0:n];
        }
#if HAVE_IO
        printf("x\ty\tz\n");
        for( size_t k=0; k<n; ++k ) {
            printf("%g\t%g\t%g\n",x[k],y[k],z[k]);
        }
#endif
        assert( __sec_reduce_add(x[0:n])==3*n );
        assert( __sec_reduce_add(y[0:n])==i*n );
        assert( __sec_reduce_add(z[0:n])==(3>=i?3-i:3+i)*n );
    }
    return 0;
}
