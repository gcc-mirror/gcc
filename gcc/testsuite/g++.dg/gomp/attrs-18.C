// { dg-do compile { target c++11 } }

#pragma omp begin declare target
#pragma omp begin declare target device_type (any)
[[omp::directive (begin declare target, device_type (host))]];
int a;
[[omp::directive (end declare target)]];
#pragma omp end declare target
#pragma omp end declare target
[[omp::directive (begin declare target device_type (nohost))]];
int b;
#pragma omp end declare target		// { dg-error "'begin declare target' in attribute syntax terminated with 'end declare target' in pragma syntax" }
#pragma omp begin declare target
int c;
[[omp::directive (end declare target)]];// { dg-error "'begin declare target' in pragma syntax terminated with 'end declare target' in attribute syntax" }
#pragma omp begin declare target device_type (host)
[[omp::directive (begin declare target)]];
int d;
#pragma omp end declare target		// { dg-error "'begin declare target' in attribute syntax terminated with 'end declare target' in pragma syntax" }
#pragma omp begin declare target
int e;
[[omp::directive (end declare target)]];// { dg-error "'begin declare target' in pragma syntax terminated with 'end declare target' in attribute syntax" }
#pragma omp end declare target
[[omp::directive (begin declare target device_type (any))]];
[[omp::directive (begin declare target)]];
int f;
#pragma omp end declare target		// { dg-error "'begin declare target' in attribute syntax terminated with 'end declare target' in pragma syntax" }
#pragma omp begin declare target
int g;
[[omp::directive (end declare target)]];// { dg-error "'begin declare target' in pragma syntax terminated with 'end declare target' in attribute syntax" }
[[omp::directive (end declare target)]];
[[omp::directive (begin declare target)]];
#pragma omp begin declare target
int h;
#pragma omp end declare target
#pragma omp end declare target		// { dg-error "'begin declare target' in attribute syntax terminated with 'end declare target' in pragma syntax" }
#pragma omp begin declare target
[[omp::directive (begin declare target)]];
int i;
[[omp::directive (end declare target)]];
[[omp::directive (end declare target)]];// { dg-error "'begin declare target' in pragma syntax terminated with 'end declare target' in attribute syntax" }
