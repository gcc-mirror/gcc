/* { dg-additional-options "-fdump-tree-original"  }  */

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


float repl1();
#pragma omp declare variant(repl1) match(construct={dispatch})
float base1();

void repl2(int *, int *);
#pragma omp declare variant(repl2) match(construct={dispatch}) adjust_args(need_device_ptr : y)
void base2(int *x, int *y);


float
dupl (int *a, int *b)
{
  omp_interop_t obj1, obj2;
  float x;

  #pragma omp dispatch interop ( obj1 ) interop(obj2)  /* { dg-error "too many 'interop' clauses" }  */
    x = base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'float repl1\\(\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch interop ( obj1) nocontext(1) interop (obj2 )  /* { dg-error "too many 'interop' clauses" }  */
    base2 (a, b);
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'base2'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'void base2\\(int\\*, int\\*\\)'" "" { target c++ } .-3 } */
  return x;
}


float
test (int *a, int *b)
{
  omp_interop_t obj1, obj2;
  float x, y;

  #pragma omp dispatch interop ( obj1 )
    x = base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'float repl1\\(\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch interop ( obj1, obj1 )  /* Twice the same - should be fine.  */
    x = base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'float repl1\\(\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch novariants(1) interop(obj2, obj1)
    y = base1 ();
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'base1'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'float base1\\(\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch interop(obj2, obj1)
    base2 (a, b);
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'repl2'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'void repl2\\(int\\*, int\\*\\)'" "" { target c++ } .-3 } */

  #pragma omp dispatch interop(obj2) nocontext(1)
    base2 (a, b);
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'base2'" "" { target c } .-2 } */
  /* { dg-error "number of list items in 'interop' clause exceeds number of 'append_args' items for 'declare variant' candidate 'void base2\\(int\\*, int\\*\\)'" "" { target c++ } .-3 } */
  return x + y;
}

/* { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj1\\)\[\\n\\r\]" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj1\\) interop\\(obj1\\)\[\\n\\r\]" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj1\\) interop\\(obj2\\) novariants\\(1\\)\[\\n\\r\]" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj1\\) interop\\(obj2\\)\[\\n\\r\]" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp dispatch nocontext\\(1\\) interop\\(obj2\\)\[\\n\\r\]" 1 "original" } } */
