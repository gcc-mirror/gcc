/* Verify that gimple-level NRV is occurring when values other than the
   return slot are call-clobbered.  */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

#ifdef __SPU__
/* SPU returns aggregates up to 1172 bytes in registers.  */
typedef struct { int x[300]; void *y; } S;
#else
typedef struct { int x[20]; void *y; } S;
#endif
typedef struct { int a; S b; } T;
S nrv_candidate (void);
void use_result (S, int);
int *ptr;
void foo (void)
{
  S result;
  T result_arr[10][5];

  int i;

  ptr = &i;

  /* i is call-clobbered for these calls, but result and result_arr
     aren't.  */
  result = nrv_candidate ();
  result_arr[3][4].b = nrv_candidate ();

  use_result (result, i);
  use_result (result_arr[3][4].b, i);
}

/* { dg-final { scan-tree-dump-times "return slot optimization" 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
