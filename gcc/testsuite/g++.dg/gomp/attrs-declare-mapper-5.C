/* { dg-do compile { target c++11 } } */

typedef struct S_ {
  int *myarr;
  int size;
} S;

[[omp::directive (declare mapper (named: struct S_ v)
		  map(to:v.size, v.myarr)
		  map(tofrom: v.myarr[0:v.size]))]];
/* { dg-note "'#pragma omp declare mapper \\(named: S_\\)' previously defined here" "" { target c++ } .-3 } */

[[omp::directive (declare mapper (named: S v)
		  map(to:v.size, v.myarr) 
		  map(tofrom: v.myarr[0:v.size]))]];
/* { dg-error "redefinition of '#pragma omp declare mapper \\(named: S\\)'" "" { target c++ } .-3 } */

[[omp::directive (declare mapper (struct S_ v)
		  map(to:v.size, v.myarr)
		  map(tofrom: v.myarr[0:v.size]))]];
/* { dg-note "'#pragma omp declare mapper \\(S_\\)' previously defined here" "" { target c++ } .-3 } */

[[omp::directive (declare mapper (S v)
		  map(to:v.size, v.myarr)
		  map(tofrom: v.myarr[0:v.size]))]];
/* { dg-error "redefinition of '#pragma omp declare mapper \\(S\\)'" "" { target c++ } .-3 } */
