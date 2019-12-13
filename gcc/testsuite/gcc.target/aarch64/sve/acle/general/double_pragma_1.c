/* { dg-do compile } */
/* { dg-options "" } */

/* It doesn't really matter if this produces errors about redefinitions,
   but it mustn't trigger an ICE.  */
#pragma GCC aarch64 "arm_sve.h"
#pragma GCC aarch64 "arm_sve.h" /* { dg-error "duplicate definition of 'arm_sve.h'" } */
