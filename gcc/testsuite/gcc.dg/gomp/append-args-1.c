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


/* (A) No prototype for the variant but for the base function.
   This is OK, the unprototyped decl is compatible with the modified
   argument list.  */

void variant_fn1();
#pragma omp declare variant(variant_fn1) match(construct={dispatch}) append_args(interop(target)) \
                                         adjust_args(need_device_ptr: x,y)
void bar1(int *x, int *y);


void variant_fn2();
#pragma omp declare variant(variant_fn2) match(construct={dispatch}) append_args(interop(target))
void bar2(int *x, int *y);



/* (B) No prototype for the variant nor for the base function.
   The declarations are compatible, but adjust_args requires a prototyped
   base function so that we know where in the arglist to insert the additional
   omp_interop_t arguments.  */

void variant_fn3();
#pragma omp declare variant(variant_fn3) match(construct={dispatch}) append_args(interop(target)) \
                                         adjust_args(need_device_ptr: x,y)
void bar3();
/* { dg-error "'x' undeclared here \\(not in a function\\)" "" { target *-*-* } .-2 }  */
/* { dg-error "'y' undeclared here \\(not in a function\\)" "" { target *-*-* } .-3 }  */
/* { dg-message "'append_args' with unprototyped base function" "" { target *-*-* } .-5 }  */


void variant_fn4();
#pragma omp declare variant(variant_fn4) match(construct={dispatch}) append_args(interop(target))
void bar4();
/* { dg-message "'append_args' with unprototyped base function" "" { target *-*-* } .-2 }  */



/* (C) Only a prototype on the variant-function side.  Again, the base
   function requires a prototype with append_args.  */

void variant_fn5(omp_interop_t, omp_interop_t);
#pragma omp declare variant(variant_fn5) match(construct={dispatch}) append_args(interop(target)) \
                                         adjust_args(need_device_ptr: x,y)
void bar5();
/* { dg-message "'append_args' with unprototyped base function" "" { target *-*-* } .-3 }  */


void variant_fn6(omp_interop_t, omp_interop_t);
#pragma omp declare variant(variant_fn6) match(construct={dispatch}) append_args(interop(target))
void bar6();
/* { dg-message "'append_args' with unprototyped base function" "" { target *-*-* } .-2 }  */


void variant_fn7(int *, int, omp_interop_t, omp_interop_t);
#pragma omp declare variant(variant_fn7) match(construct={dispatch}) append_args(interop(target))
void bar7();
/* { dg-message "'append_args' with unprototyped base function" "" { target *-*-* } .-2 }  */
