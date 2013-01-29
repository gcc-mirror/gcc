
/* { dg-do compile } */
/* { dg-options "-O3" } */

#define N 16

#include "vect-mull.x"

DEF_MULL2 (DEF_MULLB)
DEF_MULL2 (DEF_MULLH)
DEF_MULL2 (DEF_MULLS)

/* { dg-final { scan-assembler "smull\\tv\[0-9\]+\.8h"} } */
/* { dg-final { scan-assembler "smull\\tv\[0-9\]+\.4s"} } */
/* { dg-final { scan-assembler "smull\\tv\[0-9\]+\.2d"} } */
/* { dg-final { scan-assembler "umull\\tv\[0-9\]+\.8h"} } */
/* { dg-final { scan-assembler "umull\\tv\[0-9\]+\.4s"} } */
/* { dg-final { scan-assembler "umull\\tv\[0-9\]+\.2d"} } */
/* { dg-final { scan-assembler "smull2\\tv\[0-9\]+\.8h"} } */
/* { dg-final { scan-assembler "smull2\\tv\[0-9\]+\.4s"} } */
/* { dg-final { scan-assembler "smull2\\tv\[0-9\]+\.2d"} } */
/* { dg-final { scan-assembler "umull2\\tv\[0-9\]+\.8h"} } */
/* { dg-final { scan-assembler "umull2\\tv\[0-9\]+\.4s"} } */
/* { dg-final { scan-assembler "umull2\\tv\[0-9\]+\.2d"} } */
