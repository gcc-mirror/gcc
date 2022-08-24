/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int foo (void), bar (void);
extern int a;
int b;
char d;
#pragma omp declare target
long c;
#pragma omp end declare target

#pragma omp declare target (bar, a)
#pragma omp declare target to (b) link (d) enter (foo)
