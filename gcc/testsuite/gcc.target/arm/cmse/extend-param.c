/* { dg-do compile } */
/* { dg-options "-mcmse -fshort-enums" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_cmse.h>
#include <stdbool.h>

#define ARRAY_SIZE (256)
char array[ARRAY_SIZE];

enum offset
{
    zero = 0,
    one = 1,
    two = 2
};

/*
**__acle_se_unsignSecureFunc:
**	...
**	uxtb	r0, r0
**	...
*/
__attribute__((cmse_nonsecure_entry)) char unsignSecureFunc (unsigned char index) {
    if (index >= ARRAY_SIZE)
      return 0;
    return array[index];
}

/*
**__acle_se_signSecureFunc:
**	...
**	sxtb	r0, r0
**	...
*/
__attribute__((cmse_nonsecure_entry)) char signSecureFunc (signed char index) {
    if (index >= ARRAY_SIZE)
      return 0;
    return array[index];
}

/*
**__acle_se_shortUnsignSecureFunc:
**	...
**	uxth	r0, r0
**	...
*/
__attribute__((cmse_nonsecure_entry)) char shortUnsignSecureFunc (unsigned short index) {
    if (index >= ARRAY_SIZE)
      return 0;
    return array[index];
}

/*
**__acle_se_shortSignSecureFunc:
**	...
**	sxth	r0, r0
**	...
*/
__attribute__((cmse_nonsecure_entry)) char shortSignSecureFunc (signed short index) {
    if (index >= ARRAY_SIZE)
      return 0;
    return array[index];
}

/*
**__acle_se_enumSecureFunc:
**	...
**	uxtb	r0, r0
**	...
*/
__attribute__((cmse_nonsecure_entry)) char enumSecureFunc (enum offset index) {

  // Compiler may optimize away bounds check as value is an unsigned char.

  // According to AAPCS caller will zero extend to ensure value is < 256.

  if (index >= ARRAY_SIZE)
    return 0;
  return array[index];
}

/*
**__acle_se_boolSecureFunc:
**	...
**	uxtb	r0, r0
**	...
*/
__attribute__((cmse_nonsecure_entry)) char boolSecureFunc (bool index) {
  if (index >= ARRAY_SIZE)
    return 0;
  return array[index];
}

/*
**__acle_se_boolCharShortEnumSecureFunc:
**	...
**	uxtb	r0, r0
**	uxtb	r1, r1
**	uxth	r2, r2
**	uxtb	r3, r3
**	...
*/
__attribute__((cmse_nonsecure_entry,optimize(0))) char boolCharShortEnumSecureFunc (bool a, unsigned char b, unsigned short c, enum offset d) {
  size_t index = a + b + c + d;
  if (index >= ARRAY_SIZE)
    return 0;
  return array[index];
}
