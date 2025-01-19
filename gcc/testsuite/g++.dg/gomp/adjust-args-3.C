// Check that an adjust_args clause does not lead to an ICE when the match 
// clause is missing.

void f(int *, int *, int *);
#pragma omp declare variant(f) adjust_args(need_device_ptr: xxx)  /* { dg-error "an 'adjust_args' clause requires a 'match' clause" } */
void g(int *xxx, int *yyy, int *zzz);
