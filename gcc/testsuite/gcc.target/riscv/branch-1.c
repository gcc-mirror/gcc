/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */

void g();

void f(long long a) 
{
  if (a & 0xff00)
    g();
}

void f2(long long a) 
{
  if (a & (-4ull << 3))
    g();
}

void f3(long long a) 
{
  if (a & 0xffff00)
    g();
}

void f4(long long a)
{
  if (a & 0x7ff800)
    g();
}

void f5(long long a)
{
  if ((a & 0x1ff800) == 0x11c800)
    g();
}

void f6(long long a)
{
  if ((a & 0x33f800) == 0x110000)
    g();
}

/* { dg-final { scan-assembler-times "slli\t" 2 } } */
/* { dg-final { scan-assembler-times "srli\t" 5 } } */
/* { dg-final { scan-assembler-times "andi\t" 3 } } */
/* { dg-final { scan-assembler-times "\tli\t" 3 } } */
/* { dg-final { scan-assembler-not "addi\t" } } */
/* { dg-final { scan-assembler-not "and\t" } } */

