/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

static int arr[10];
static int g (int a) { return -a; }

void f (int *ptr, float x) {

  /* Implicit conversion float -> bool */
  #pragma omp metadirective when(user={condition(x)} : nothing) otherwise(nothing)

  /* Implicit conversion pointer -> bool */
  #pragma omp metadirective when(user={condition(ptr)} : nothing) otherwise(nothing)

  /* Array expression undergoes array->pointer conversion, OK but test is
     always optimized away.  */
  #pragma omp metadirective when(user={condition(arr)} : nothing) otherwise(nothing)

  /* Function reference has pointer-to-function type, OK but test is
     always optimized away.  */
  #pragma omp metadirective when(user={condition(g)} : nothing) otherwise(nothing)
}

/* { dg-final { scan-tree-dump "x != 0.0" "original" } } */
/* { dg-final { scan-tree-dump "ptr != 0B" "original" } } */
