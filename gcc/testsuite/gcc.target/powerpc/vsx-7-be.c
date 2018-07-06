/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx" } */

/* This is an extension of altivec-7-be.c, with vsx target features included. */

/* Expected results for Big Endian:
(from altivec-7.h)
     vec_packpx                     vpkpx
     vec_ld                         lxvd2x or lxv
     vec_lde                        lvewx
     vec_ldl                        lxvl
     vec_lvewx                      lvewx
     vec_andc                       xxnor
                                    xxland
     vec_vxor                       xxlxor
     vec_vmsumubm                   vmsumubm
     vec_vmulesb                    vmulesb
     vec_vmulosb                    vmulosb
(from vsx-7.h)
     vec_unpackl                    vupkhsh
     vec_unpackh                    vupklsh
*/

/* { dg-final { scan-assembler-times "vpkpx" 2 } } */
/* { dg-final { scan-assembler-times "vmulesb" 1 } } */
/* { dg-final { scan-assembler-times "vmulosb" 1 } } */

// For LE platforms P9 and later, we generate the lxv insn instead of lxvd2x.
/* { dg-final { scan-assembler-times {\mlxvd2x\M}  0  { target { { powerpc64*le-*-* } && { p9vector_hw } } } } } */
/* { dg-final { scan-assembler-times {\mlxv\M}    36  { target { { powerpc64*le-*-* } && { p9vector_hw } } } } } */
// For LE platforms < P9.
/* { dg-final { scan-assembler-times {\mlxvd2x\M}  36  { target { { powerpc64*le-*-* } && { ! p9vector_hw } } } } } */
// For BE platforms we generate 6 lxvd2x insns.
/* { dg-final { scan-assembler-times {\mlxvd2x\M}  6  { target { { ! powerpc64*le-*-* } && { ! p9vector_hw } } } } } */

/* { dg-final { scan-assembler-times "lvewx" 2 } } */
/* { dg-final { scan-assembler-times "lvxl" 1 } } */
/* { dg-final { scan-assembler-times "vupklsh" 1 } } */
/* { dg-final { scan-assembler-times "vupkhsh" 1 } } */
/* { dg-final { scan-assembler-times "xxlnor" 4 } } */
/* { dg-final { scan-assembler-times "xxland" 4 } } */
/* { dg-final { scan-assembler-times "xxlxor" 5 } } */
/* { dg-final { scan-assembler-times "vupkhpx" 1 } } */

/* Source code for the 'altivec' test in altivec-7.h */
/* Source code for the 'vsx' required tests in vsx-7.h */

#include "altivec-7.h"
#include "vsx-7.h"
