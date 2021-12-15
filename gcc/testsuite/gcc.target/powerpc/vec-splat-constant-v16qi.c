/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <altivec.h>

/* Test whether XXSPLTIW is generated for V16HI vector constants where the
   first 4 elements are the same as the next 4 elements, etc.  */

vector unsigned char
v16qi_const_1 (void)
{
  return (vector unsigned char) { 1, 1, 1, 1, 1, 1, 1, 1,
				  1, 1, 1, 1, 1, 1, 1, 1, }; /* VSLTPISB.  */
}

vector unsigned char
v16qi_const_2 (void)
{
  return (vector unsigned char) { 1, 2, 3, 4, 1, 2, 3, 4,
				  1, 2, 3, 4, 1, 2, 3, 4, }; /* XXSPLTIW.  */
}

/* { dg-final { scan-assembler-times {\mxxspltiw\M}              1 } } */
/* { dg-final { scan-assembler-times {\mvspltisb\M|\mxxspltib\M} 1 } } */
/* { dg-final { scan-assembler-not   {\mlxvx?\M}                   } } */
/* { dg-final { scan-assembler-not   {\mplxv\M}                    } } */
