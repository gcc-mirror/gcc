/* Test parsing of OMP clause adjust_args */
/* { dg-do compile } */

int f0 (void *a);
int g (void *a);
int f1 (int);

#pragma omp declare variant (f0) match (construct={target}) adjust_args (nothing: a) /* { dg-error "an 'adjust_args' clause can only be specified if the 'dispatch' selector of the construct selector set appears in the 'match' clause" } */
int f2 (void *a);
#pragma omp declare variant (f0) match (construct={dispatch,target}) adjust_args (need_device_ptr: a) /* { dg-error "'int f0.void..' used as a variant with incompatible 'construct' selector sets" } */
int f2a (void *a);
#pragma omp declare variant (f0) match (construct={target,dispatch}) adjust_args (need_device_ptr: a) /* { dg-error "'int f0.void..' used as a variant with incompatible 'construct' selector sets" } */
int f2b (void *a);
#pragma omp declare variant (f0) match (construct={dispatch},device={arch(gcn)}) adjust_args (need_device_ptr: a) /* { dg-error "'int f0.void..' used as a variant with incompatible 'construct' selector sets" } */
int f2c (void *a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (other: a) /* { dg-error "expected 'nothing', 'need_device_ptr' or 'need_device_addr'" } */
int f3 (int a);
#pragma omp declare variant (f0) adjust_args (nothing: a) /* { dg-error "an 'adjust_args' clause requires a 'match' clause" } */
int f4 (void *a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args () /* { dg-error "expected 'nothing', 'need_device_ptr' or 'need_device_addr' followed by ':'" } */
int f5 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing) /* { dg-error "expected 'nothing', 'need_device_ptr' or 'need_device_addr' followed by ':'" } */
int f6 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing:) /* { dg-error "expected primary-expression before '\\)' token" } */
int f7 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing: z) /* { dg-error "'z' is not a function parameter" } */
int f8 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (need_device_ptr: a) /* { dg-note "parameter specified here" } */
int f9 (int a); /* { dg-error "parameter specified in an 'adjust_args' clause with the 'need_device_ptr' modifier must be of pointer type" } */
#pragma omp declare variant (f1) match (construct={dispatch}) \
				 adjust_args (nothing: a)     \
				 adjust_args (nothing: a)
int f10 (int a);
/* { dg-note "parameter previously specified here" "" { target *-*-* } .-3 } */
/* { dg-error "OpenMP parameter list items must specify a unique parameter" "" { target *-*-* } .-3 } */
#pragma omp declare variant (g) match (construct={dispatch}) \
				adjust_args (nothing: a)     \
				adjust_args (need_device_ptr: a)
int f11 (void *a);
/* { dg-note "parameter previously specified here" "" { target *-*-* } .-3 } */
/* { dg-error "OpenMP parameter list items must specify a unique parameter" "" { target *-*-* } .-3 } */

int b;

#pragma omp declare variant (g) match (construct={dispatch}) adjust_args (need_device_ptr: b) /* { dg-error "'b' is not a function parameter" } */
int f12 (void *a);
#pragma omp declare variant (g) match (construct={dispatch}) adjust_args (need_device_ptr: this) /* { dg-error "expected unqualified-id, integer, or expression before 'this'" } */
int f13 (void *a);
