/* Verify that NRV optimizations are prohibited when the LHS is an
   indirect reference to something that may be call-clobbered. */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

typedef struct { int x[20]; void *y; } S;
S nrv_candidate (void);
void use_result (S);
void make_escape (S *);
S global_S;
void foo (void)
{
  S *result;
  S local_S;

  /* We can't perform return slot optimization because global_S is
     global and may be clobbered by nrv_candidate.  */
  result = &global_S;
  *result = nrv_candidate ();
  use_result (*result);

  /* We can't perform return slot optimization because local_S is
     call_clobbered (its address escapes prior to invoking
     nrv_candidate).  */
  make_escape (&local_S);
  result = &local_S;
  *result = nrv_candidate ();
  use_result (*result);
}

/* { dg-final { scan-tree-dump-times "return slot optimization" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

