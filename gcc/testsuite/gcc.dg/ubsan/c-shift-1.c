/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -w" } */
/* { dg-shouldfail "ubsan" } */

int x;
int
main (void)
{
  /* None of the following should pass.  */
  int A[1 >> -1] = {};    /* { dg-error "variable-sized object may not be initialized" } */
  int B[-1 >> -1] = {};   /* { dg-error "variable-sized object may not be initialized" } */
  int D[1 << -1] = {};    /* { dg-error "variable-sized object may not be initialized" } */
  int E[-1 << -1] = {};   /* { dg-error "variable-sized object may not be initialized" } */
  int F[-1 >> 200] = {};  /* { dg-error "variable-sized object may not be initialized" } */
  int G[1 << 200] = {};   /* { dg-error "variable-sized object may not be initialized" } */

  return 0;
}
