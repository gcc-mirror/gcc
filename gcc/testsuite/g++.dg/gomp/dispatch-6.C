/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

/* Check that templates are properly handled. */

template<typename T>
T templ_var_fn(T x);

#pragma omp declare variant(templ_var_fn) match(construct={dispatch}) adjust_args(need_device_ptr: x)
template<typename T>
T templ_base_fn(T x);

template<typename T, typename TB>
void f(int *y, TB x)
{
#pragma omp dispatch nocontext (x)
  y = templ_base_fn<T> (y); 
#pragma omp dispatch novariants (x)
  y = templ_base_fn<T> (y); 
}

void bar()
{
  int a;
  bool b = true;
  f<int*,bool> (&a, b);
}

/* { dg-final { scan-tree-dump-times "y = \.GOMP_DISPATCH \\(templ_base_fn<int\\*> \\(y\\)\\)" 2 "original" } } */
