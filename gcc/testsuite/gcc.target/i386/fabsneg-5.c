/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

/* A volatile value in memory is changed in place too: the access has the
   width of the value, as for other read-modify-write operations.  */

void abs_f (volatile float *p) { *p = __builtin_fabsf (*p); }
void neg_f (volatile float *p) { *p = -*p; }
void nabs_f (volatile float *p) { *p = -__builtin_fabsf (*p); }
void abs_d (volatile double *p) { *p = __builtin_fabs (*p); }
void neg_d (volatile double *p) { *p = -*p; }
void nabs_d (volatile double *p) { *p = -__builtin_fabs (*p); }

/* But not when only the read or only the write is volatile: the single
   access could not be both.  */

void neg_rf (float *p) { *p = -*(volatile float *) p; }
void neg_wf (float *p) { *(volatile float *) p = -*p; }
void neg_rd (double *p) { *p = -*(volatile double *) p; }
void neg_wd (double *p) { *(volatile double *) p = -*p; }

/* { dg-final { scan-assembler-times "andl\t\\\$2147483647, \\(" 1 } } */
/* { dg-final { scan-assembler-times "xorl\t\\\$-2147483648, \\(" 1 } } */
/* { dg-final { scan-assembler-times "\torl\t\\\$-2147483648, \\(" 1 } } */
/* { dg-final { scan-assembler-times "btrq\t\\\$63, \\(" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "btcq\t\\\$63, \\(" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "btsq\t\\\$63, \\(" 1 { target { ! ia32 } } } } */
