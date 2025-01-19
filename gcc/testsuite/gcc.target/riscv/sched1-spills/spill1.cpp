/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -save-temps -fverbose-asm" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "O1" "-Og" "-Os" "-Oz" } } */

/* Reduced from SPEC2017 Cactu ML_BSSN_Advect.cpp
   by comparing -fschedule-insn and -fno-schedule-insns builds.
   Shows up one extra spill (pair of spill markers "sfp") in verbose asm
   output which the patch fixes.  */

void s();
double b, c, d, e, f, g, h, k, l, m, n, o, p, q, t, u, v;
int *j;
double *r, *w;
long x;
void y() {
  double *a((double *)s);
  for (;;)
    for (; j[1];)
      for (int i = 1; i < j[0]; i++) {
        k = l;
        m = n;
        o = p = q;
        r[0] = t;
        a[0] = u;
        x = g;
        e = f;
        v = w[x];
        b = c;
        d = h;
        }
}

/* { dg-final { scan-assembler-not "%sfp" } } */
