/* { dg-do compile } */

typedef struct S_ {
  int *myarr;
  int size;
} S;

#pragma omp declare mapper (named: struct S_ v) map(to:v.size, v.myarr) \
						map(tofrom: v.myarr[0:v.size])
/* { dg-error "previous '#pragma omp declare mapper'" "" { target c } .-2 } */
/* { dg-note "'#pragma omp declare mapper \\(named: S_\\)' previously defined here" "" { target c++ } .-3 } */

#pragma omp declare mapper (named: S v) map(to:v.size, v.myarr) \
					map(tofrom: v.myarr[0:v.size])
/* { dg-error "redeclaration of 'named' '#pragma omp declare mapper' for type 'S' \\\{aka 'struct S_'\\\}" "" { target c } .-2 } */
/* { dg-error "redefinition of '#pragma omp declare mapper \\(named: S\\)'" "" { target c++ } .-3 } */

#pragma omp declare mapper (struct S_ v) map(to:v.size, v.myarr) \
					 map(tofrom: v.myarr[0:v.size])
/* { dg-error "previous '#pragma omp declare mapper'" "" { target c } .-2 } */
/* { dg-note "'#pragma omp declare mapper \\(S_\\)' previously defined here" "" { target c++ } .-3 } */

#pragma omp declare mapper (S v) map(to:v.size, v.myarr) \
				 map(tofrom: v.myarr[0:v.size])
/* { dg-error "redeclaration of '<default>' '#pragma omp declare mapper' for type 'S' \\\{aka 'struct S_'\\\}" "" { target c } .-2 } */
/* { dg-error "redefinition of '#pragma omp declare mapper \\(S\\)'" "" { target c++ } .-3 } */
