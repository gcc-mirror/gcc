/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mcpu=power7 -ffast-math" } */
/* { dg-final { scan-assembler-times "fctiwz" 2 } } */
/* { dg-final { scan-assembler-times "fctiwuz" 2 } } */
/* { dg-final { scan-assembler-times "fctidz" 1 } } */
/* { dg-final { scan-assembler-times "fctiduz" 1 } } */
/* { dg-final { scan-assembler-times "xscvdpsxds" 1 } } */
/* { dg-final { scan-assembler-times "xscvdpuxds" 1 } } */

void float_to_int  (int *dest, float  src) { *dest = (int) src; }
void double_to_int (int *dest, double src) { *dest = (int) src; }

void float_to_uint  (int *dest, float  src) { *dest = (unsigned int) src; }
void double_to_uint (int *dest, double src) { *dest = (unsigned int) src; }

void float_to_llong  (long long *dest, float  src) { *dest = (long long) src; }
void double_to_llong (long long *dest, double src) { *dest = (long long) src; }

void float_to_ullong  (unsigned long long *dest, float  src) { *dest = (unsigned long long) src; }
void double_to_ullong (unsigned long long *dest, double src) { *dest = (unsigned long long) src; }
