/* Test parsing of #pragma omp declare variant */
/* { dg-do compile } */

int fn0 (int);
int fn6 (int);

#pragma omp declare variant (fn0) match (user={condition(0)})
int a;	/* { dg-error "not immediately followed by a function declaration or definition" } */

#pragma omp declare variant (fn0) match (user={condition(0)})
int fn1 (int a), fn2 (int a);	/* { dg-error "not immediately followed by a single function declaration or definition" } */

#pragma omp declare variant (fn0) match (user={condition(0)})
int b, fn3 (int a);	/* { dg-error "not immediately followed by a function declaration or definition" } */

#pragma omp declare variant (fn0) match (user={condition(0)})
int fn4 (int a), c;	/* { dg-error "not immediately followed by a function declaration or definition" } */

int t;

#pragma omp declare variant (fn0) match (user={condition(0)})
#pragma omp declare variant (fn6) match (implementation={vendor(unknown)})
#pragma omp threadprivate(t)	/* { dg-error "must be followed by function declaration or definition or another" } */
int fn5 (int a);

#pragma omp declare variant (1 + 2) match (user={condition(0)})	/* { dg-error "expected identifier before numeric constant" } */
int fn7 (int);

#pragma omp declare variant (t) match (user={condition(0)})	/* { dg-error "variant 't' is not a function" } */
int fn8 (int);

long fn9 (char, short);

#pragma omp declare variant (fn9) match (implementation={vendor(unknown)})	/* { dg-error "variant 'fn9' and base 'fn10' have incompatible types" } */
int fn10 (int, long long);

#pragma omp declare variant (memcpy) match (implementation={vendor(llvm)})	/* { dg-error "'memcpy' undeclared here" } */
void *fn11 (void *, const void *, __SIZE_TYPE__);

#pragma omp declare variant (__builtin_memmove) match (implementation={vendor(gnu)})	/* { dg-error "variant '__builtin_memmove' is a built-in" } */
void *fn12 (void *, const void *, __SIZE_TYPE__);
