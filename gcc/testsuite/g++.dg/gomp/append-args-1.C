/* The following definitions are in omp_lib, which cannot be included
gcc/testsuite/g++.dg/gomp/append-args-1.C   in gcc/testsuite/  */

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

template<typename T, typename T2>
float repl1(T, T2, T2);
#pragma omp declare variant(repl1) match(construct={dispatch}) append_args(interop(target,prefer_type(1,5,4)), interop(targetsync))
template<typename T>
float base1(T);

/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'float repl1\\(T, T2, T2\\) \\\[with T = short int; T2 = omp_interop_t\\\]'" "" { target *-*-* } .-4 }  */
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'float repl1\\(T, T2, T2\\) \\\[with T = omp_interop_t; T2 = omp_interop_t\\\]'" "" { target *-*-* } .-5 }  */
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'float repl1\\(T, T2, T2\\) \\\[with T = float; T2 = omp_interop_t\\\]'" "" { target *-*-* } .-6 }  */



template<typename T, typename T2, typename T3>
void repl3inval(T, T2, float);
#pragma omp declare variant(repl3inval) match(construct={dispatch}) adjust_args(nothing : y) \
        append_args(interop(prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")}),target,targetsync))
template<typename T, typename T2>
void base2inval(T x, T2 y);

/* { dg-error "no matching function for call to 'repl3inval\\(int\\*, omp_interop_t, omp_interop_t\\)'" "" { target *-*-* } .-5 }  */
/* { dg-note "there is 1 candidate" "" { target *-*-* } .-6 }  */
/* { dg-note "candidate 1: 'template<class T, class T2, class T3> void repl3inval\\(T, T2, float\\)'" "" { target *-*-* } .-8 }  */
/* { dg-note "template argument deduction/substitution failed:" "" { target *-*-* } .-9 }  */
/* { dg-note "couldn't deduce template parameter 'T3'" "" { target *-*-* } .-9 }  */


template<typename T>
void repl99(T);
#pragma omp declare variant(repl99) match(construct={dispatch}) \
        append_args(interop(target, targetsync, prefer_type("cuda")))
void base99();

/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'void repl99\\(T\\) \\\[with T = omp_interop_t\\\]'" "" { target *-*-* } .-3 }  */



template<typename T, typename T2, typename T3>
void repl2(T, T2, T3, T3);
#pragma omp declare variant(repl2) match(construct={dispatch}) adjust_args(need_device_ptr : y) \
        append_args(interop(target, targetsync, prefer_type(1)), \
                    interop(prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")})))
template<typename T, typename T2>
void base2(T x, T2 y);

/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'void repl2\\(T, T2, T3, T3\\) \\\[with T = int\\*; T2 = int\\*; T3 = omp_interop_t\\\]'" "" { target *-*-* } .-5 }  */
/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'void repl2\\(T, T2, T3, T3\\) \\\[with T = int\\*; T2 = omp_interop_t; T3 = omp_interop_t\\\]'" "" { target *-*-* } .-6 }  */


template<typename T,typename T3>
void tooFewRepl(T, T, T3);
#pragma omp declare variant(tooFewRepl) match(construct={dispatch}) \
        append_args(interop(target, targetsync, prefer_type(1)), \
                    interop(prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")})))
template<typename T, typename T2>
void tooFewBase(T x, T2 y);

/* { dg-error "no matching function for call to 'tooFewRepl\\(int\\*, int\\*, omp_interop_t, omp_interop_t\\)'" "" { target *-*-* } .-6 }  */
/* { dg-note "there is 1 candidate" "" { target *-*-* } .-7 }  */
/* { dg-note "candidate 1: 'template<class T, class T3> void tooFewRepl\\(T, T, T3\\)'" "" { target *-*-* } .-9 }  */
/* { dg-note "candidate expects 3 arguments, 4 provided" "" { target *-*-* } .-10 }  */



template<typename T, typename T2>
void repl3(T, T2, ...);
#pragma omp declare variant(repl3) match(construct={dispatch}) \
        append_args(interop(prefer_type("cuda", "hsa")))
template<typename T>
void base3(T, ...);

/* { dg-message "sorry, unimplemented: 'append_args' clause not yet supported for 'void repl3\\(T, T2, \.\.\.\\) \\\[with T = int\\*; T2 = omp_interop_t\\\]'" "" { target *-*-* } .-4 }  */



float
test (int *a, int *b)
{
  omp_interop_t obj1, obj2;
  float x, y;

  #pragma omp dispatch interop ( obj1, obj2 )
    x = base1<short> (5);
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch
    base2inval<int *, omp_interop_t> (a, omp_interop_none);

  #pragma omp dispatch
    base99 ();
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch interop ( obj1 )
    base2<int *, omp_interop_t> (b, omp_interop_none);
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch interop ( obj1 )
    base2<int *, int *> (b, a);
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch interop ( obj1 )
    x = base1<omp_interop_t> (omp_interop_none);
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch interop ( obj1 )
    x = base1<float> (1.0f);
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  #pragma omp dispatch
    tooFewBase<int*,int*>(a,b);

  #pragma omp dispatch nocontext(1)
    base3<int*>(a, 1, 2, "abc");

  #pragma omp dispatch
    base3<int*>(a, 1, 2, "abc");
  /* { dg-note "required by 'dispatch' construct" "" { target *-*-* } .-2 }  */

  return x;
}
