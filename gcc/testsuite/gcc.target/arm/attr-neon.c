/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-add-options arm_neon arm_v8_vfp } */
/* The arm_v8_vfp adds -mfpu=fp-armv8 to the command line, overriding any
   -mfpu= option set by arm_neon, thus ensuring that the attributes below
   really are checked for correct fpu selection.  */

/* Verify that neon instructions are emitted once.  */
void __attribute__ ((target("fpu=neon")))
 f1(int n, int x[], int y[]) {
  int i;
  for (i = 0; i < n; ++i)
    y[i] = x[i] << 3;
}

void __attribute__ ((target("fpu=vfp")))
f3(int n, int x[], int y[]) {
  int i;
  for (i = 0; i < n; ++i)
    y[i] = x[i] << 3;
}

/* { dg-final { scan-assembler-times "\.fpu\\s+vfp\n" 1 } } */
/* { dg-final { scan-assembler-times "\.fpu\\s+neon\n" 1 } } */
/* { dg-final { scan-assembler-times "vshl" 1 } } */
