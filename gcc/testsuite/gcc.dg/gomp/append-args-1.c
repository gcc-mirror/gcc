/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

/* The errors might be a bit questionable, but still a resonable solution
   for questionable code ...  */

/* For all empty args, assume C < C23; in C++/C23 it becomes the same as '…(void)'.  */

/* This uses append_args, once with adjust_args and once without.  */

typedef enum omp_interop_t
{
  omp_interop_none = 0,
  __omp_interop_t_max__ = __UINTPTR_MAX__
} omp_interop_t;


/* (A) No prototype for the variant but for the base function.  */

void variant_fn1();
#pragma omp declare variant(variant_fn1) match(construct={dispatch}) append_args(interop(target)) \
                                         adjust_args(need_device_ptr: x,y)
void bar1(int *x, int *y);
/* { dg-error "variant 'variant_fn1' and base 'bar1' have incompatible types" "" { target *-*-* } .-3 }  */


void variant_fn2();
#pragma omp declare variant(variant_fn2) match(construct={dispatch}) append_args(interop(target))
void bar2(int *x, int *y);
/* { dg-error "variant 'variant_fn2' and base 'bar2' have incompatible types" "" { target *-*-* } .-2 }  */



/* (B) No prototype for the variant nor for the base function.  */

void variant_fn3();  /* { dg-error "argument 1 of 'variant_fn3' must be of 'omp_interop_t'" }  */
#pragma omp declare variant(variant_fn3) match(construct={dispatch}) append_args(interop(target)) \
                                         adjust_args(need_device_ptr: x,y)
void bar3();
/* { dg-error "'x' undeclared here \\(not in a function\\)" "" { target *-*-* } .-2 }  */
/* { dg-error "'y' undeclared here \\(not in a function\\)" "" { target *-*-* } .-3 }  */
/* { dg-note "'append_args' specified here" "" { target *-*-* } .-5 }  */


void variant_fn4();  /* { dg-error "argument 1 of 'variant_fn4' must be of 'omp_interop_t'" }  */
#pragma omp declare variant(variant_fn4) match(construct={dispatch}) append_args(interop(target))
void bar4();
/* { dg-note "'append_args' specified here" "" { target *-*-* } .-2 }  */



/* (C) Only a prototype on the variant-function side.  */

void variant_fn5(omp_interop_t, omp_interop_t);
#pragma omp declare variant(variant_fn5) match(construct={dispatch}) append_args(interop(target)) \
                                         adjust_args(need_device_ptr: x,y)
void bar5();
/* { dg-error "variant 'variant_fn5' and base 'bar5' have incompatible types" "" { target *-*-* } .-3 }  */


void variant_fn6(omp_interop_t, omp_interop_t);
#pragma omp declare variant(variant_fn6) match(construct={dispatch}) append_args(interop(target))
void bar6();
/* { dg-error "variant 'variant_fn6' and base 'bar6' have incompatible types" "" { target *-*-* } .-2 }  */


void variant_fn7(int *, int, omp_interop_t, omp_interop_t);
#pragma omp declare variant(variant_fn7) match(construct={dispatch}) append_args(interop(target))
void bar7();
/* { dg-error "variant 'variant_fn7' and base 'bar7' have incompatible types" "" { target *-*-* } .-2 }  */
