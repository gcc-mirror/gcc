/* The following definitions are in omp_lib, which cannot be included
   in gcc/testsuite/  */

#if __cplusplus >= 201103L
# define __GOMP_UINTPTR_T_ENUM : __UINTPTR_TYPE__
#else
# define __GOMP_UINTPTR_T_ENUM
#endif

typedef enum omp_interop_t __GOMP_UINTPTR_T_ENUM
{
  omp_interop_none = 0,
  __omp_interop_t_max__ = __UINTPTR_MAX__
} omp_interop_t;

float repl0(short, short);
#pragma omp declare variant(repl0) match(construct={dispatch}) append_args(interop(target), interop(targetsync))
float base0();
/* { dg-error "argument 1 of 'repl0' must be of 'omp_interop_t'" "" { target c } .-3 }  */
/* { dg-error "argument 1 of 'float repl0\\(short int, short int\\)' must be of 'omp_interop_t'" "" { target c++ } .-4 }  */
/* { dg-note "'append_args' specified here" "" { target *-*-* } .-4 } */

float repl1(omp_interop_t, omp_interop_t);
#pragma omp declare variant(repl1) match(construct={dispatch}) append_args(interop(target), interop(targetsync))
float base1();
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'repl1', except when specifying all 2 objects in the 'interop' clause of the 'dispatch' directive" "" { target c } .-2 }  */
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'float repl1\\(omp_interop_t, omp_interop_t\\)', except when specifying all 2 objects in the 'interop' clause of the 'dispatch' directive" "" { target c++ } .-3 }  */

void repl2(int *, int *, omp_interop_t, omp_interop_t);
#pragma omp declare variant(repl2) match(construct={dispatch}) adjust_args(need_device_ptr : y) \
        append_args(interop(target, targetsync, prefer_type(1)), \
                    interop(prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")})))
void base2(int *x, int *y);
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'repl2', except when specifying all 2 objects in the 'interop' clause of the 'dispatch' directive" "" { target c } .-3 }  */
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'void repl2\\(int\\*, int\\*, omp_interop_t, omp_interop_t\\)', except when specifying all 2 objects in the 'interop' clause of the 'dispatch' directive" "" { target c++ } .-4 }  */

void repl3(int, omp_interop_t, ...);
#pragma omp declare variant(repl3) match(construct={dispatch}) \
        append_args(interop(prefer_type("cuda", "hsa")))
void base3(int, ...);
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'repl3', except when specifying all 1 objects in the 'interop' clause of the 'dispatch' directive" "" { target c } .-2 }  */
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'void repl3\\(int, omp_interop_t, \\.\\.\\.\\)', except when specifying all 1 objects in the 'interop' clause of the 'dispatch' directive" "" { target c++ } .-3 }  */
/* { dg-note "'declare variant' candidate 'repl3' declared here" "" { target c } .-4 } */
/* { dg-note "'declare variant' candidate 'void repl3\\(int, omp_interop_t, \\.\\.\\.\\)' declared here" "" { target c++ } .-5 } */

float repl4(short, short, omp_interop_t, short);
#pragma omp declare variant(repl4) match(construct={dispatch}) append_args(interop(target)) append_args(interop(targetsync))  /* { dg-error "too many 'append_args' clauses" } */
float base4(short, short);
/* { dg-error "variant 'repl4' and base 'base4' have incompatible types" "" { target c } .-2 }  */
/* { dg-error "too few arguments to function 'float repl4\\(short int, short int, omp_interop_t, short int\\)'" "" { target c++ } .-3 }  */
/* { dg-note "declared here" "" { target c++ } .-5 } */


float repl5(short, short, omp_interop_t, short);
#pragma omp declare variant(repl5) match(construct={dispatch}) append_args(interop(target),interop(targetsync))
float base5(short, short);
/* { dg-error "argument 4 of 'repl5' must be of 'omp_interop_t'" "" { target c } .-3 }  */
/* { dg-error "argument 4 of 'float repl5\\(short int, short int, omp_interop_t, short int\\)' must be of 'omp_interop_t'" "" { target c++ } .-4 }  */
/* { dg-note "'append_args' specified here" "" { target *-*-* } .-4 } */


float repl6(short, short, omp_interop_t, short);
#pragma omp declare variant(repl6) match(construct={dispatch}) append_args(interop(target))
float base6(short, short);
/* { dg-error "variant 'repl6' and base 'base6' have incompatible types" "" { target c } .-2 }  */
/* { dg-error "too few arguments to function 'float repl6\\(short int, short int, omp_interop_t, short int\\)'" "" { target c++ } .-3 }  */
/* { dg-note "declared here" "" { target c++ } .-5 } */


float
test (int *a, int *b)
{
  omp_interop_t obj1, obj2;
  float x;

  #pragma omp dispatch interop ( obj1 )
    x = base1 ();
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch interop ( obj1 )
    base2 (a, b);
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch
    base3 (5, 1, 2, 3);
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch interop (obj2)
    base3 (5, 1, 2, 3);

  #pragma omp dispatch interop (obj2, obj1) /* { dg-error "the 'device' clause must be present if the 'interop' clause has more than one list item" } */
    base3 (5, 1, 2, 3);
  /* { dg-error "number of list items in 'interop' clause \\(2\\) exceeds the number of 'append_args' items \\(1\\) for 'declare variant' candidate 'repl3'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause \\(2\\) exceeds the number of 'append_args' items \\(1\\) for 'declare variant' candidate 'void repl3\\(int, omp_interop_t, \\.\\.\\.\\)'" "" { target c++ } .-3 } */
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-4 }  */

  return x;
}
