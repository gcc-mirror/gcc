/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -mpower8-vector -mabi=ibmlongdouble -Wno-psabi" } */

/* Check that complex multiply generates the right call when long double is
   IBM extended double floating point.  */

typedef _Complex long double cld_t;

void
multiply (cld_t *p, cld_t *q, cld_t *r)
{
  *p = *q * *r;
}

/* { dg-final { scan-assembler "bl __multc3" } } */
