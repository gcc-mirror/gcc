/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

extern int a;
#pragma omp declare target
#pragma omp declare target to (a)		/* { dg-error "with clauses in between" } */
#pragma omp end declare target
int b;
#pragma omp declare target to (b) link (b)	/* { dg-error "specified both in declare target" } */
int c;
#pragma omp declare target (c)
#pragma omp declare target link (c)		/* { dg-error "specified both in declare target" } */
int foo (void);
#pragma omp declare target link (foo)		/* { dg-error "is not a variable in clause" } */
struct S;
extern struct S d[];				/* { dg-error "array type has incomplete element type" "" { target c } } */
#pragma omp declare target to (d)		/* { dg-error "does not have a mappable type in" } */
extern struct S e;
#pragma omp declare target link (e)		/* { dg-error "does not have a mappable type in" } */
extern int f[];
#pragma omp declare target to (f)		/* { dg-error "does not have a mappable type in" } */
int g, h;
#pragma omp threadprivate (g, h)
#pragma omp declare target to (g)		/* { dg-error "is threadprivate variable in" } */
#pragma omp declare target link (h)		/* { dg-error "is threadprivate variable in" } */
int j[10];
#pragma omp declare target to (j[0:4])		/* { dg-error "expected" } */
