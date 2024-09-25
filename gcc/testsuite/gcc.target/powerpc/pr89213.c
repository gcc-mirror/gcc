/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-options "-mcpu=power9 -O2" } */

/* Optimize vector shifts by constants.  */

#include <altivec.h>

typedef vector long long vi64_t;
typedef vector unsigned long long vui64_t;

typedef vector int vi32_t;
typedef vector unsigned int vui32_t;

vi64_t
shiftra_test64_4 (vi64_t a)
{
  vui64_t x = {4, 4};
  return (vi64_t) vec_vsrad (a, x);
}

vi64_t
shiftrl_test64_4 (vi64_t a)
{
  vui64_t x = {4, 4};
  return (vi64_t) vec_vsrd (a, x);
}

vi64_t
shiftl_test64_4 (vi64_t a)
{
  vui64_t x = {4, 4};
  return (vi64_t) vec_vsld (a, x);
}

vi64_t
shiftra_test64_29 (vi64_t a)
{
  vui64_t x = {29, 29};
  return (vi64_t) vec_vsrad (a, x);
}

vi64_t
shiftrl_test64_29 (vi64_t a)
{
  vui64_t x = {29, 29};
  return (vi64_t) vec_vsrd (a, x);
}

vi64_t
shiftl_test64_29 (vi64_t a)
{
  vui64_t x = {29, 29};
  return (vi64_t) vec_vsld (a, x);
}

vi32_t
shiftra_test32_4 (vi32_t a)
{
  vui32_t x = {4, 4, 4, 4};
  return (vi32_t) vec_vsraw (a, x);
}

vi32_t
shiftrl_test32_4 (vi32_t a)
{
  vui32_t x = {4, 4, 4, 4};
  return (vi32_t) vec_vsrw (a, x);
}

vi32_t
shiftl_test32_4 (vi32_t a)
{
  vui32_t x = {4, 4, 4, 4};
  return (vi32_t) vec_vslw (a, x);
}

vi32_t
shiftra_test32_29 (vi32_t a)
{
  vui32_t x = {29, 29, 29, 29};
  return (vi32_t) vec_vsraw (a, x);
}

vi32_t
shiftrl_test32_29 (vi32_t a)
{
  vui32_t x = {29, 29, 29, 29};
  return (vi32_t) vec_vsrw (a, x);
}

vi32_t
shiftl_test32_29 (vi32_t a)
{
  vui32_t x = {29, 29, 29, 29};
  return (vi32_t) vec_vslw (a, x);
}

/* { dg-final { scan-assembler-times {\mxxspltib\M}  6 } } */
/* { dg-final { scan-assembler-times {\mvsld\M}      2 } } */
/* { dg-final { scan-assembler-times {\mvslw\M}      2 } } */
/* { dg-final { scan-assembler-times {\mvspltisw\M}  6 } } */
/* { dg-final { scan-assembler-times {\mvsrd\M}      2 } } */
/* { dg-final { scan-assembler-times {\mvsrw\M}      2 } } */
/* { dg-final { scan-assembler-times {\mvsrad\M}     2 } } */
/* { dg-final { scan-assembler-times {\mvsraw\M}     2 } } */
