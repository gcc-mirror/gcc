/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -mfloat128" } */

#include <math.h>

#ifndef TYPE
#define TYPE __float128
#endif

TYPE from_double (double a) { return (TYPE)a; }
TYPE from_single (float a) { return (TYPE)a; }

TYPE from_double_load (double *a) { return (TYPE)*a; }
TYPE from_single_load (float *a) { return (TYPE)*a; }

double to_double (TYPE a) { return (double)a; }
float to_single (TYPE a) { return (float)a; }

void to_double_store (TYPE a, double *p) { *p = (double)a; }
void to_single_store (TYPE a, float *p) { *p = (float)a; }

TYPE from_sign_char (signed char a) { return (TYPE)a; }
TYPE from_sign_short (short a) { return (TYPE)a; }
TYPE from_sign_int (int a) { return (TYPE)a; }
TYPE from_sign_long (long a) { return (TYPE)a; }

TYPE from_sign_char_load (signed char *a) { return (TYPE)*a; }
TYPE from_sign_short_load (short *a) { return (TYPE)*a; }
TYPE from_sign_int_load (int *a) { return (TYPE)*a; }
TYPE from_sign_long_load (long *a) { return (TYPE)*a; }

TYPE from_sign_char_load_4 (signed char *a) { return (TYPE)a[4]; }
TYPE from_sign_short_load_4 (short *a) { return (TYPE)a[4]; }
TYPE from_sign_int_load_4 (int *a) { return (TYPE)a[4]; }
TYPE from_sign_long_load_4 (long *a) { return (TYPE)a[4]; }

TYPE from_sign_char_load_n (signed char *a, long n) { return (TYPE)a[n]; }
TYPE from_sign_short_load_n (short *a, long n) { return (TYPE)a[n]; }
TYPE from_sign_int_load_n (int *a, long n) { return (TYPE)a[n]; }
TYPE from_sign_long_load_n (long *a, long n) { return (TYPE)a[n]; }

signed char to_sign_char (TYPE a) { return (signed char)a; }
short to_sign_short (TYPE a) { return (short)a; }
int to_sign_int (TYPE a) { return (int)a; }
long to_sign_long (TYPE a) { return (long)a; }

void to_sign_char_store (TYPE a, signed char *p) { *p = (signed char)a; }
void to_sign_short_store (TYPE a, short *p) { *p = (short)a; }
void to_sign_int_store (TYPE a, int *p) { *p = (int)a; }
void to_sign_long_store (TYPE a, long *p) { *p = (long)a; }

void to_sign_char_store_4 (TYPE a, signed char *p) { p[4] = (signed char)a; }
void to_sign_short_store_4 (TYPE a, short *p) { p[4] = (short)a; }
void to_sign_int_store_4 (TYPE a, int *p) { p[4] = (int)a; }
void to_sign_long_store_4 (TYPE a, long *p) { p[4] = (long)a; }

void to_sign_char_store_n (TYPE a, signed char *p, long n) { p[n] = (signed char)a; }
void to_sign_short_store_n (TYPE a, short *p, long n) { p[n] = (short)a; }
void to_sign_int_store_n (TYPE a, int *p, long n) { p[n] = (int)a; }
void to_sign_long_store_n (TYPE a, long *p, long n) { p[n] = (long)a; }

TYPE from_uns_char (unsigned char a) { return (TYPE)a; }
TYPE from_uns_short (unsigned short a) { return (TYPE)a; }
TYPE from_uns_int (unsigned int a) { return (TYPE)a; }
TYPE from_uns_long (unsigned long a) { return (TYPE)a; }

TYPE from_uns_char_load (unsigned char *a) { return (TYPE)*a; }
TYPE from_uns_short_load (unsigned short *a) { return (TYPE)*a; }
TYPE from_uns_int_load (unsigned int *a) { return (TYPE)*a; }
TYPE from_uns_long_load (unsigned long *a) { return (TYPE)*a; }

TYPE from_uns_char_load_4 (unsigned char *a) { return (TYPE)a[4]; }
TYPE from_uns_short_load_4 (unsigned short *a) { return (TYPE)a[4]; }
TYPE from_uns_int_load_4 (unsigned int *a) { return (TYPE)a[4]; }
TYPE from_uns_long_load_4 (unsigned long *a) { return (TYPE)a[4]; }

TYPE from_uns_char_load_n (unsigned char *a, long n) { return (TYPE)a[n]; }
TYPE from_uns_short_load_n (unsigned short *a, long n) { return (TYPE)a[n]; }
TYPE from_uns_int_load_n (unsigned int *a, long n) { return (TYPE)a[n]; }
TYPE from_uns_long_load_n (unsigned long *a, long n) { return (TYPE)a[n]; }

unsigned char to_uns_char (TYPE a) { return (unsigned char)a; }
unsigned short to_uns_short (TYPE a) { return (unsigned short)a; }
unsigned int to_uns_int (TYPE a) { return (unsigned int)a; }
unsigned long to_uns_long (TYPE a) { return (unsigned long)a; }

void to_uns_char_store (TYPE a, unsigned char *p) { *p = (unsigned char)a; }
void to_uns_short_store (TYPE a, unsigned short *p) { *p = (unsigned short)a; }
void to_uns_int_store (TYPE a, unsigned int *p) { *p = (unsigned int)a; }
void to_uns_long_store (TYPE a, unsigned long *p) { *p = (unsigned long)a; }

void to_uns_char_store_4 (TYPE a, unsigned char *p) { p[4] = (unsigned char)a; }
void to_uns_short_store_4 (TYPE a, unsigned short *p) { p[4] = (unsigned short)a; }
void to_uns_int_store_4 (TYPE a, unsigned int *p) { p[4] = (unsigned int)a; }
void to_uns_long_store_4 (TYPE a, unsigned long *p) { p[4] = (unsigned long)a; }

void to_uns_char_store_n (TYPE a, unsigned char *p, long n) { p[n] = (unsigned char)a; }
void to_uns_short_store_n (TYPE a, unsigned short *p, long n) { p[n] = (unsigned short)a; }
void to_uns_int_store_n (TYPE a, unsigned int *p, long n) { p[n] = (unsigned int)a; }
void to_uns_long_store_n (TYPE a, unsigned long *p, long n) { p[n] = (unsigned long)a; }

/* { dg-final { scan-assembler-not {\mbl __}       } } */
/* { dg-final { scan-assembler     {\mxscvdpqp\M}  } } */
/* { dg-final { scan-assembler     {\mxscvqpdp\M}  } } */
/* { dg-final { scan-assembler     {\mxscvqpdpo\M} } } */
/* { dg-final { scan-assembler     {\mxscvqpsdz\M} } } */
/* { dg-final { scan-assembler     {\mxscvqpswz\M} } } */
/* { dg-final { scan-assembler     {\mxscvsdqp\M}  } } */
/* { dg-final { scan-assembler     {\mxscvudqp\M}  } } */
/* { dg-final { scan-assembler     {\mlxsd\M}      } } */
/* { dg-final { scan-assembler     {\mlxsiwax\M}   } } */
/* { dg-final { scan-assembler     {\mlxsiwzx\M}   } } */
/* { dg-final { scan-assembler     {\mlxssp\M}     } } */
/* { dg-final { scan-assembler     {\mstxsd\M}     } } */
/* { dg-final { scan-assembler     {\mstxsiwx\M}   } } */
/* { dg-final { scan-assembler     {\mstxssp\M}    } } */
