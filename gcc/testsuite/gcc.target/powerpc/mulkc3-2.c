/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -mvsx -mabi=ieeelongdouble -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

/* Check that complex multiply generates the right call when long double is
   IEEE 128-bit floating point.  */

typedef _Complex long double cld_t;

void
multiply (cld_t *p, cld_t *q, cld_t *r)
{
  *p = *q * *r;
}

/* { dg-final { scan-assembler "bl __mulkc3" } } */
