/* { dg-do compile } */
/* { dg-options "-march=rv64i -march=unset -mcpu=sifive-x280 -march=rv64i -mabi=lp64 -misa-spec=20191213" } */
int foo()
{
}

/* { dg-final { scan-assembler "\.attribute arch, \"rv64i2p1\"" } } */
