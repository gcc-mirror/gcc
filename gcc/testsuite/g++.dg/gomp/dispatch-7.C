// { dg-do compile { target c++11 } }


template<typename T>
T templ_var_fn(T x);


template<typename T> [[omp::directive (declare variant(templ_var_fn) match(construct={dispatch}) adjust_args(need_device_ptr: x))]]
T templ_base_fn(T x);

template<typename T, typename TB>
void f(int *y, TB x)
{
[[omp::directive (dispatch nocontext (x))]]
  y = templ_base_fn<T> (y); 
[[omp::directive (dispatch novariants (x))]]
  y = templ_base_fn<T> (y); 
}

void bar()
{
  int a;
  bool b = true;
  f<int*,bool> (&a, b);
}


void variant_fn(int *x, int *y, int *z);

[[omp::directive (declare variant(variant_fn) match(construct={dispatch}) adjust_args(need_device_ptr: x,y) adjust_args(nothing: z))]]
void bar(int *x, int *y, int *z);

void sub(int *x, int *y, int *z)
{
  [[omp::directive (dispatch is_device_ptr(y))]]
     bar(x, y, z);
  [[omp::directive (dispatch device(0))]]
     bar(x, y, z);
  [[omp::directive (dispatch nocontext(1) novariants(1))]]
     bar(x, y, z);
  [[omp::directive (dispatch depend(inout: x) nowait)]]
     bar(x, y, z);
}
