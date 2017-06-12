/* { dg-do compile { target { powerpc*-*-linux* && powerpc_altivec_ok } } } */
/* { dg-options "-maltivec" } */

/* GNU extension: PowerPC AltiVec Built-in Functions.
   These should be rejected as invalid AltiVec types.  */

__vector _Decimal32 vd32;		/* { dg-error "AltiVec types" } */
__vector _Decimal64 vd64;		/* { dg-error "AltiVec types" } */
__vector _Decimal128 vd128;		/* { dg-error "AltiVec types" } */
