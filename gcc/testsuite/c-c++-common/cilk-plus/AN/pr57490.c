/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

const int n = 8;
float x[8], y[8], z[8];
int main() {
    int i = 0;
    float x_sum =0;
    for(i=1; i<=5; i+=4 ) {
        x[0:n] = 3;
        y[0:n] = i;
        z[0:n] = 0;
        (void)((__sec_reduce_add(x[0:n])==3*n) || (__builtin_abort (), 0));
        (void)((__sec_reduce_add(y[0:n])==i*n) || (__builtin_abort (), 0));
        (void)((__sec_reduce_add(z[0:n])==0) || (__builtin_abort (), 0));

        if (x[0:n] >= y[0:n]) {
            z[0:n] = x[0:n] - y[0:n];
        } else {
            z[0:n] = x[0:n] + y[0:n];
        }
        (void)((__sec_reduce_add(x[0:n])==3*n) || (__builtin_abort (), 0));
        (void)((__sec_reduce_add(y[0:n])==i*n) || (__builtin_abort (), 0));
        (void)((__sec_reduce_add(z[0:n])==(3>=i?3-i:3+i)*n) 
	       || (__builtin_abort (), 0));
    }
    return 0;
}
