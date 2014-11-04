/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */
/* { dg-options "-O2 -ffast-math -mcpu=power5 -mabi=altivec" } */
/* { dg-final { scan-assembler-times "fabs" 3 } } */
/* { dg-final { scan-assembler-times "fnabs" 3 } } */
/* { dg-final { scan-assembler-times "fsel" 3 } } */
/* { dg-final { scan-assembler-times "fcpsgn\|xscpsgndp" 4 } } */

/* fabs/fnabs/fsel */
double normal1 (double a, double b) { return __builtin_copysign (a, b); }

#pragma GCC push_options
#pragma GCC target ("cpu=power5")
/* fabs/fnabs/fsel */
double power5 (double a, double b) { return __builtin_copysign (a, b); }
#pragma GCC pop_options

#pragma GCC target ("cpu=power6")
/* fcpsgn */
double power6 (double a, double b) { return __builtin_copysign (a, b); }
#pragma GCC reset_options

#pragma GCC target ("cpu=power6x")
/* fcpsgn */
double power6x (double a, double b) { return __builtin_copysign (a, b); }
#pragma GCC reset_options

#pragma GCC target ("cpu=power7")
/* xscpsgndp */
double power7 (double a, double b) { return __builtin_copysign (a, b); }
#pragma GCC reset_options

#pragma GCC target ("cpu=power7,no-vsx")
/* fcpsgn */
double power7n (double a, double b) { return __builtin_copysign (a, b); }
#pragma GCC reset_options

/* fabs/fnabs/fsel */
double normal2 (double a, double b) { return __builtin_copysign (a, b); }
