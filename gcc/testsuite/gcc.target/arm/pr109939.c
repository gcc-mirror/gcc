/* { dg-do compile } */
/* { dg-require-effective-target arm_sat_ok } */
/* { dg-add-options arm_sat } */
/* { dg-additional-options "-O -Wall -Wconversion" } */

#include <arm_acle.h>

int dbg_ssat_out;
int dbg_ssat_in;

void test_arm_ssat(void)
{
    dbg_ssat_out = __ssat(dbg_ssat_in, 16);
}
