/* Test parsing of OMP clause adjust_args */
/* { dg-do compile } */

int b;

int f0 (void *a);
int g (void *a);
int f1 (int);

#pragma omp declare variant (f0) match (construct={target}) adjust_args (nothing: a) /* { dg-error "an 'adjust_args' clause can only be specified if the 'dispatch' selector of the 'construct' selector set appears in the 'match' clause" } */
int f2 (void *a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (other: a) /* { dg-error "expected 'nothing' or 'need_device_ptr'" } */
int f3 (int a);
#pragma omp declare variant (f0) adjust_args (nothing: a) /* { dg-error "an 'adjust_args' clause requires a 'match' clause" } */
int f4 (void *a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args () /* { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" } */
int f5 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing) /* { dg-error "expected 'nothing' or 'need_device_ptr' followed by ':'" } */
int f6 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing:) /* { dg-error "expected identifier before '\\)' token" } */
int f7 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing: z) /* { dg-error "'z' undeclared here \\(not in a function\\)" } */
int f8 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (need_device_ptr: a) /* { dg-error "'a' is not of pointer type" } */
int f9 (int a);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing: a) adjust_args (nothing: a) /* { dg-error "'a' is specified more than once" } */
int f10 (int a);
#pragma omp declare variant (g) match (construct={dispatch}) adjust_args (nothing: a) adjust_args (need_device_ptr: a) /* { dg-error "'a' is specified more than once" } */
int f11 (void *a);
#pragma omp declare variant (g) match (construct={dispatch}) adjust_args (need_device_ptr: b) /* { dg-error "'b' is not a function argument" } */
int f12 (void *a);

