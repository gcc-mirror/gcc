/* { dg-options "-O2 -mstrict-align" } */

#include "ldp_stp_5.c"

/* { dg-final { scan-assembler-times {stp\tq[0-9]+, q[0-9]} 3 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {str\tq[0-9]+} 1 { xfail *-*-* } } } */
