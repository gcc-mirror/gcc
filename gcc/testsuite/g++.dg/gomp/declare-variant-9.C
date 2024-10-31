/* { dg-additional-options "-fdump-tree-gimple" } */
int &variant_fn();

#pragma omp declare variant(variant_fn) match(user={condition(1)})
int &bar();

void sub(int &a)
{
  bar();
  a = bar(); 
}

template<typename T>
T &templ_var_fn(T x);

#pragma omp declare variant(templ_var_fn) match(user={condition(1)})
template<typename T>
T &templ_base_fn(T x);

void run(int &b)
{
  templ_base_fn<int>(5);
  b = templ_base_fn<int>(7); 
}

/* { dg-final { scan-tree-dump "  variant_fn \\(\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "  _1 = variant_fn \\(\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "  templ_var_fn<int> \\(5\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "  _1 = templ_var_fn<int> \\(7\\);" "gimple" } } */
