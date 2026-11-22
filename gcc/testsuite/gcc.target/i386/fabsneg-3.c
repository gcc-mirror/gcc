/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

/* fabs, negation and -fabs of a value in memory, stored back in place,
   change the sign bit in memory without loading the value.  For double only
   with 64-bit integers: otherwise the upper word would be changed alone,
   which stalls a following load of the whole value.  */

void abs_f (float *p) { *p = __builtin_fabsf (*p); }
void neg_f (float *p) { *p = -*p; }
void abs_d (double *p) { *p = __builtin_fabs (*p); }
void neg_d (double *p) { *p = -*p; }
void nabs_f (float *p) { *p = -__builtin_fabsf (*p); }
void nabs_d (double *p) { *p = -__builtin_fabs (*p); }

/* { dg-final { scan-assembler-times "andl\t\\\$2147483647, \\(" 1 } } */
/* { dg-final { scan-assembler-times "xorl\t\\\$-2147483648, \\(" 1 } } */
/* { dg-final { scan-assembler-times "\torl\t\\\$-2147483648, \\(" 1 } } */
/* { dg-final { scan-assembler-times "btrq\t\\\$63, \\(" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "btcq\t\\\$63, \\(" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "btsq\t\\\$63, \\(" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "movs\[sd\]|andp\[sd\]|xorp\[sd\]|orp\[sd\]" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "\tf(ld|st|abs|chs)" { target { ! ia32 } } } } */
