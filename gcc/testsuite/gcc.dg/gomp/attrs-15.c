/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c23" } */

#pragma omp begin assumes absent (target)
#pragma omp begin assumes absent (target)
[[omp::directive (begin assumes absent (target))]];
int a;
[[omp::directive (end assumes)]];
#pragma omp end assumes
#pragma omp end assumes
[[omp::directive (begin assumes absent (target))]];
int b;
#pragma omp end assumes		/* { dg-error "'begin assumes' in attribute syntax terminated with 'end assumes' in pragma syntax" } */
#pragma omp begin assumes absent (target)
int c;
[[omp::directive (end assumes)]];/* { dg-error "'begin assumes' in pragma syntax terminated with 'end assumes' in attribute syntax" } */
#pragma omp begin assumes absent (target)
[[omp::directive (begin assumes absent (target))]];
int d;
#pragma omp end assumes		/* { dg-error "'begin assumes' in attribute syntax terminated with 'end assumes' in pragma syntax" } */
#pragma omp begin assumes absent (target)
int e;
[[omp::directive (end assumes)]];/* { dg-error "'begin assumes' in pragma syntax terminated with 'end assumes' in attribute syntax" } */
#pragma omp end assumes
[[omp::directive (begin assumes absent (target))]];
[[omp::directive (begin assumes absent (target))]];
int f;
#pragma omp end assumes		/* { dg-error "'begin assumes' in attribute syntax terminated with 'end assumes' in pragma syntax" } */
#pragma omp begin assumes absent (target)
int g;
[[omp::directive (end assumes)]];/* { dg-error "'begin assumes' in pragma syntax terminated with 'end assumes' in attribute syntax" } */
[[omp::directive (end assumes)]];
[[omp::directive (begin assumes absent (target))]];
#pragma omp begin assumes absent (target)
int h;
#pragma omp end assumes
#pragma omp end assumes		/* { dg-error "'begin assumes' in attribute syntax terminated with 'end assumes' in pragma syntax" } */
#pragma omp begin assumes absent (target)
[[omp::directive (begin assumes absent (target))]];
int i;
[[omp::directive (end assumes)]];
[[omp::directive (end assumes)]];/* { dg-error "'begin assumes' in pragma syntax terminated with 'end assumes' in attribute syntax" } */
