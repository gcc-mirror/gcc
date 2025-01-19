/* Testing PACBTI multilibs matches without mve.  */
/* { dg-do run } */
/* { dg-require-effective-target arm_pacbti_hw } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "" } { "-mfloat-abi=hard" } } */
/* { dg-options "-mcpu=unset -march=armv8.1-m.main+dsp+fp.dp+pacbti -mbranch-protection=standard -mthumb -mfloat-abi=hard" } */

#include "pac.h"
