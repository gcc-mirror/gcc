/* { dg-do compile { target rv32 } } */
/* { dg-options "-Os -march=rv32imafd_zca_zcmp -mabi=ilp32d -mcmodel=medlow" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O2" "-Og" "-O3" "-Oz" "-flto" } } */
/* { dg-final { scan-assembler "cm\\.push" } } */
/* { dg-final { scan-assembler-not "fsd\\s+fs\[0-9\]+,-" } } */
/*
   When Zcmp (cm.push) is enabled, GCC may allocate only the GPR save area
   with cm.push and then save the callee-saved FPRs (fs0..fs11) at *negative*
   offsets of the current SP, deferring the rest of the frame allocation to a
   trailing "addi sp,sp,-N".  Those below-SP slots are treated as scratch by
   the interrupt/preemption machinery (e.g. nested interrupts that run on the
   interrupted SP), so they can be silently clobbered, corrupting the
   callee-saved FP registers.  The prologue must grow the frame before
   emitting any callee-saved FPR store, so no "fsd fsN,-...".  */

double gmem[40];

__attribute__((noinline)) double
helper (double a, double b)
{
  return a * b + gmem[0];
}

__attribute__((noinline)) double
sink (double x)
{
  gmem[3] = x;
  return x;
}

/* Keep many callee-saved FPRs live across calls so that GCC must spill
   fs2..fs11; this is what triggers the below-SP saves with cm.push.  */
double
test_zcmp_fpr_save (double a, double b, double c, double d, double e,
		    double q, double w, double r, double t, double y,
		    double u, double i)
{
  double x0 = a, x1 = b, x2 = c, x3 = d, x4 = e;
  double x5 = q, x6 = w, x7 = r, x8 = t, x9 = y;

  double acc = helper (x0, x1) + helper (x2, x3) + helper (x4, x5)
	       + helper (x6, x7) + helper (x8, x9);
  double acc2 = helper (x0, x2) + helper (x4, x6) + helper (x8, x9)
		+ helper (w, t) + helper (r, y) + helper (u, i);
  double acc3 = helper (x1, x3) + helper (x5, x7) + helper (x9, y)
		+ helper (q, w) + helper (t, r) + helper (x0, x6);

  gmem[1] = acc;
  gmem[2] = acc2;
  gmem[4] = acc3;
  return sink (acc + acc2 + acc3);
}
