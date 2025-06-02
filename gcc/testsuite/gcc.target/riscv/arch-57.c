/* { dg-do compile } */
/* { dg-options "-march=rv64i_smdbltrp -mabi=lp64" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_zicsr2p0_smdbltrp1p0\"" } } */
