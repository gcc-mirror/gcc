/* Ensure that a scalar floating-point value that is live across a SIMT_ENTER
   transition reads the same in every lane.

   There are no scalar (SALU) instructions for floating-point values, so a
   double naturally lives in a VGPR.  A VGPR written while EXEC == 1 holds a
   valid value in lane 0 only, so the simt_vect pass must make it uniform at
   the SIMT_ENTER, where EXEC widens to -1.

   The store to GOUT below stays scalar (uniform address), but a scalar store
   still executes in every lane, so if the stored value is not uniform then
   some other lane's garbage wins and GOUT gets the wrong value.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1 -fdump-rtl-simt_vect" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)

volatile double vin = 3.5;
double gout;
double dirty[64];

/* Leave per-lane garbage in the vector registers, so that the lanes that
   must be fixed up at SIMT_ENTER cannot happen to read as zero.  This has
   its own SIMT region so that MAIN is left with exactly one.  */

void __attribute__((noinline,noclone))
dirty_vgprs (void)
{
  int A = __builtin_gcn_simt_enter (1, 2);
  dirty[TID] = TID * 1.0e6 + 1.0;
  __builtin_gcn_simt_exit (A);
}

/* Note: main enters in single-threaded mode.  */
int
main (void)
{
  double x;
  int A;

  dirty_vgprs ();

  /* Computed with EXEC == 1, so only lane 0 is valid here.  */
  x = vin * 2.0 + 1.0;

  A = __builtin_gcn_simt_enter (1, 2);
  gout = x;
  __builtin_gcn_simt_exit (A);

  /* Use single-threading to check the result because broken multithread
     might also have broken checking.  */
  if (gout != 8.0)
    __builtin_exit (1);

  return 0;
}

/* Confirm the run above is actually testing the fixup, rather than passing
   because X never reached a VGPR in the first place.  */
/* { dg-final { scan-rtl-dump "r\[0-9\]+:DF=unspec\\\[r\[0-9\]+:DF\\\]ENSURE_SCALAR" "simt_vect" } } */
