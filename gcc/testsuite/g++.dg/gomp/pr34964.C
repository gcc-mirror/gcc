// PR c++/34964
// { dg-do compile }
// { dg-options "-fopenmp" }

char x[] = 0;	// { dg-error "initializer fails to determine size" }
#pragma omp threadprivate (x)
