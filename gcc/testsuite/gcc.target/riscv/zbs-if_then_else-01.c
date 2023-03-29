/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */

void g();

void f1 (long a)
{
  if ((a & ((1ul << 33) | (1 << 4))) == (1ul << 33))
    g();
}

void f2 (long a)
{
  if ((a & 0x12) == 0x10)
    g();
}

/* { dg-final { scan-assembler-times "bexti\t" 2 } } */
/* { dg-final { scan-assembler-times "andn\t" 1 } } */
