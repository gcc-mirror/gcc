// { dg-do compile { target c++11 } }

#pragma omp declare target
#pragma omp declare target
[[omp::directive (declare target)]];
int a;
[[omp::directive (end declare target)]];
#pragma omp end declare target
#pragma omp end declare target
[[omp::directive (declare target)]];
int b;
#pragma omp end declare target		// { dg-error "'declare target' in attribute syntax terminated with 'end declare target' in pragma syntax" }
#pragma omp declare target
int c;
[[omp::directive (end declare target)]];// { dg-error "'declare target' in pragma syntax terminated with 'end declare target' in attribute syntax" }
#pragma omp declare target
[[omp::directive (declare target)]];
int d;
#pragma omp end declare target		// { dg-error "'declare target' in attribute syntax terminated with 'end declare target' in pragma syntax" }
#pragma omp declare target
int e;
[[omp::directive (end declare target)]];// { dg-error "'declare target' in pragma syntax terminated with 'end declare target' in attribute syntax" }
#pragma omp end declare target
[[omp::directive (declare target)]];
[[omp::directive (declare target)]];
int f;
#pragma omp end declare target		// { dg-error "'declare target' in attribute syntax terminated with 'end declare target' in pragma syntax" }
#pragma omp declare target
int g;
[[omp::directive (end declare target)]];// { dg-error "'declare target' in pragma syntax terminated with 'end declare target' in attribute syntax" }
[[omp::directive (end declare target)]];
[[omp::directive (declare target)]];
#pragma omp declare target
int h;
#pragma omp end declare target
#pragma omp end declare target		// { dg-error "'declare target' in attribute syntax terminated with 'end declare target' in pragma syntax" }
#pragma omp declare target
[[omp::directive (declare target)]];
int i;
[[omp::directive (end declare target)]];
[[omp::directive (end declare target)]];// { dg-error "'declare target' in pragma syntax terminated with 'end declare target' in attribute syntax" }
