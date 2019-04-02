/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Expected results for Big Endian:
     vec_packpx                     vpkpx
     vec_ld                         lxvd2x
     vec_lde                        lvewx
     vec_ldl                        lxvl
     vec_lvewx                      lvewx
     vec_unpackh                    vupklsh
     vec_unpackl                    vupkhsh
     vec_andc                       xxnor
                                    xxland
     vec_vxor                       xxlxor
     vec_vmsumubm                   vmsumubm
     vec_vmulesb                    vmulesb
     vec_vmulosb                    vmulosb
*/

/* { dg-final { scan-assembler-times "vpkpx" 2 } } */
/* { dg-final { scan-assembler-times "vmulesb" 1 } } */
/* { dg-final { scan-assembler-times "vmulosb" 1 } } */
/* { dg-final { scan-assembler-times "lvewx" 2 } } */
/* { dg-final { scan-assembler-times "lvxl" 1 } } */
/* { dg-final { scan-assembler-times "vupklsh" 1 } } */
/* { dg-final { scan-assembler-times "vupkhsh" 1 } } */

/* Source code for the test in altivec-7.h */
#include "altivec-7.h"
