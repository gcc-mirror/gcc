/* { dg-do compile }  */

int x, y, z, a, b;
#pragma omp groupprivate(a,b)  // { dg-message "sorry, unimplemented: 'omp groupprivate'" }
#pragma omp groupprivate(x) device_type(any)     // { dg-message "sorry, unimplemented: 'omp groupprivate'" }
#pragma omp groupprivate(y) device_type(host)    // { dg-message "sorry, unimplemented: 'omp groupprivate'" }
#pragma omp groupprivate(z) device_type(nohost)  // { dg-message "sorry, unimplemented: 'omp groupprivate'" }

[[omp::decl (groupprivate)]] int d, e;  // { dg-message "sorry, unimplemented: 'omp groupprivate'" }
[[omp::decl (groupprivate,device_type(any))]] int f1;     // { dg-message "sorry, unimplemented: 'omp groupprivate'" }
[[omp::decl (groupprivate,device_type(host))]] int f2;    // { dg-message "sorry, unimplemented: 'omp groupprivate'" }
[[omp::decl (groupprivate,device_type(nohost))]] int f3;  // { dg-message "sorry, unimplemented: 'omp groupprivate'" }
