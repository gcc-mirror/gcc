/* { dg-additional-options "-fdump-tree-original" } */

struct S
{
  int *ptr;
  float f;
};

int a[32], b[32];
#pragma acc declare copyin(readonly: a) copyin(b)
/* Not visible in 'original' dump; handled via 'offload_vars'.  */

int main (void)
{
  int x[32], y[32];
  struct S s = {x, 0};

  #pragma acc declare copyin(readonly: x/*[:32]*/, s/*.ptr[:16]*/) copyin(y/*[:32]*/)

  #pragma acc parallel copyin(readonly: x[:32], s.ptr[:16]) copyin(y[:32])
  {
    #pragma acc cache (readonly: x[:32])
    #pragma acc cache (y[:32])
  }

  #pragma acc kernels copyin(readonly: x[:32], s.ptr[:16]) copyin(y[:32])
  {
    #pragma acc cache (readonly: x[:32])
    #pragma acc cache (y[:32])
  }

  #pragma acc serial copyin(readonly: x[:32], s.ptr[:16]) copyin(y[:32])
  {
    #pragma acc cache (readonly: x[:32])
    #pragma acc cache (y[:32])
  }

  #pragma acc data copyin(readonly: x[:32], s.ptr[:16]) copyin(y[:32])
  {
    #pragma acc cache (readonly: x[:32])
    #pragma acc cache (y[:32])
  }

  #pragma acc enter data copyin(readonly: x[:32], s.ptr[:16]) copyin(y[:32])

  return 0;
}

/* { dg-final { scan-tree-dump-times "(?n)#pragma acc declare map\\(to:y\\) map\\(readonly,to:s\\) map\\(readonly,to:x\\)" 1 "original" } } */

/* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:\\*s.ptr \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c } } } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc kernels map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:\\*s.ptr \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c } } } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc serial map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:\\*s.ptr \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c } } } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc data map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) map\\(readonly,to:\\*s.ptr \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c } } } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc enter data map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) map\\(readonly,to:\\*s.ptr \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c } } } } */

/* { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:\\*NON_LVALUE_EXPR <s.ptr> \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c++ } } } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc kernels map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:\\*NON_LVALUE_EXPR <s.ptr> \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c++ } } } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc serial map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:\\*NON_LVALUE_EXPR <s.ptr> \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c++ } } } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc data map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) map\\(readonly,to:\\*NON_LVALUE_EXPR <s.ptr> \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c++ } } } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc enter data map\\(to:y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\) map\\(readonly,to:\\*NON_LVALUE_EXPR <s.ptr> \\\[len: \[0-9\]+\\\]\\) .+ map\\(readonly,to:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\)" 1 "original" { target { c++ } } } } */

/* { dg-final { scan-tree-dump-times "(?n)#pragma acc cache \\(readonly:x\\\[0\\\] \\\[len: \[0-9\]+\\\]\\);$" 4 "original" } } */
/* { dg-final { scan-tree-dump-times "(?n)#pragma acc cache \\(y\\\[0\\\] \\\[len: \[0-9\]+\\\]\\);$" 4 "original" } } */
