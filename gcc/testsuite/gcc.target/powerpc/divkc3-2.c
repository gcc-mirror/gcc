/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -mpower8-vector -mabi=ieeelongdouble -Wno-psabi" } */

/* Check that complex multiply generates the right call when long double is
   IEEE 128-bit floating point.  */

typedef _Complex long double cld_t;

void
divide (cld_t *p, cld_t *q, cld_t *r)
{
  *p = *q / *r;
}

/* { dg-final { scan-assembler "bl __divkc3" } } */
