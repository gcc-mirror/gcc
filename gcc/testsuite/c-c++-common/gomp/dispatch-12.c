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


void repl1();
#pragma omp declare variant(repl1) match(construct={dispatch})
void base1();

void
test ()
{
  const omp_interop_t obj1 = omp_interop_none;
  omp_interop_t obj2[2];
  const omp_interop_t *obj3;
  short x;

  #pragma omp dispatch interop ( obj1, obj2, obj1 )  /* { dg-error "'obj2' must be of 'omp_interop_t'" }  */
    base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'void repl1\\(\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch interop ( obj3 )  /* { dg-error "'obj3' must be of 'omp_interop_t'" }  */
    base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'void repl1\\(\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch interop ( obj1 )
    base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'void repl1\\(\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch interop ( obj2 )  /* { dg-error "'obj2' must be of 'omp_interop_t'" }  */
    base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'void repl1\\(\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch interop ( x )  /* { dg-error "'x' must be of 'omp_interop_t'" }  */
    base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'void repl1\\(\\)'" "" { target c++ } .-3 } */
}
