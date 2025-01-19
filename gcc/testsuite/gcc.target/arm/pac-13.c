/* Testing PACBTI multilib matches.  */
/* { dg-do run } */
/* { dg-require-effective-target arm_pacbti_hw } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "" } { "-mfloat-abi=hard" } } */
/* { dg-options "-mcpu=unset -march=armv8.1-m.main+mve.fp+fp.dp+pacbti -mbranch-protection=standard -mthumb -mfloat-abi=hard" } */

#include "pac.h"
