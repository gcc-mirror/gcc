/* { dg-do compile } */
/* { dg-options "-march=rv64i_ssdbltrp -mabi=lp64" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_zicsr2p0_ssdbltrp1p0\"" } } */
