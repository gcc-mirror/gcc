/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

#ifndef TYPE
#define TYPE _Float128
#endif

/* Test the code generation of the various _Float128 operations.  */
TYPE f128_add (TYPE a, TYPE b) { return a+b; }
TYPE f128_sub (TYPE a, TYPE b) { return a-b; }
TYPE f128_mul (TYPE a, TYPE b) { return a*b; }
TYPE f128_div (TYPE a, TYPE b) { return a/b; }
TYPE f128_fma (TYPE a, TYPE b, TYPE c) { return (a*b)+c; }
TYPE f128_fms (TYPE a, TYPE b, TYPE c) { return (a*b)-c; }
TYPE f128_nfma (TYPE a, TYPE b, TYPE c) { return -((a*b)+c); }
TYPE f128_nfms (TYPE a, TYPE b, TYPE c) { return -((a*b)-c); }
TYPE f128_neg (TYPE a) { return -a; }

long f128_cmove (TYPE a, TYPE b, long c, long d) { return (a == b) ? c : d; }

double f128_to_double (TYPE a) { return (double)a; }
float f128_to_float (TYPE a) { return (float)a; }
long f128_to_long (TYPE a) { return (long)a; }
unsigned long f128_to_ulong (TYPE a) { return (unsigned long)a; }
int f128_to_int (TYPE a) { return (int)a; }
unsigned int f128_to_uint (TYPE a) { return (unsigned int)a; }

TYPE double_to_f128 (double a) { return (TYPE)a; }
TYPE float_to_f128 (float a) { return (TYPE)a; }
TYPE long_to_f128 (long a) { return (TYPE)a; }
TYPE ulong_to_f128 (unsigned long a) { return (TYPE)a; }
TYPE int_to_f128 (int a) { return (TYPE)a; }
TYPE uint_to_f128 (unsigned int a) { return (TYPE)a; }

/* { dg-final { scan-assembler     {\mmfvsrd\M}    } } */
/* { dg-final { scan-assembler     {\mmfvsrwz\M}   } } */
/* { dg-final { scan-assembler     {\mmtvsrd\M}    } } */
/* { dg-final { scan-assembler     {\mmtvsrwa\M}   } } */
/* { dg-final { scan-assembler 	   {\mxscmpuqp\M}  } } */
/* { dg-final { scan-assembler 	   {\mxscvdpqp\M}  } } */
/* { dg-final { scan-assembler 	   {\mxscvqpdp\M}  } } */
/* { dg-final { scan-assembler 	   {\mxscvqpdpo\M} } } */
/* { dg-final { scan-assembler 	   {\mxscvqpsdz\M} } } */
/* { dg-final { scan-assembler 	   {\mxscvqpswz\M} } } */
/* { dg-final { scan-assembler 	   {\mxscvqpudz\M} } } */
/* { dg-final { scan-assembler 	   {\mxscvqpuwz\M} } } */
/* { dg-final { scan-assembler 	   {\mxscvsdqp\M}  } } */
/* { dg-final { scan-assembler 	   {\mxscvudqp\M}  } } */
/* { dg-final { scan-assembler 	   {\mxsdivqp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsmaddqp\M}  } } */
/* { dg-final { scan-assembler 	   {\mxsmsubqp\M}  } } */
/* { dg-final { scan-assembler 	   {\mxsmulqp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsnegqp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsnmaddqp\M} } } */
/* { dg-final { scan-assembler 	   {\mxsnmsubqp\M} } } */
/* { dg-final { scan-assembler 	   {\mxssubqp\M}   } } */
/* { dg-final { scan-assembler-not {\mbl\M}        } } */

