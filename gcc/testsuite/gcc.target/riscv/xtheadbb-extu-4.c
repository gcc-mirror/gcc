/* { dg-do compile { target { rv32 && { ! riscv_abi_e } } } } */
/* { dg-options "-march=rv32gc_xtheadbb" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Og" "-Oz" } } */

struct c {
  int f : 25;
} d;

int b;
extern unsigned int e[];

void g()
{
  d.f = e[2] >> (b << ~4194303 + 4194332) - 58096371;
}

/* { dg-final { scan-assembler-not {th.extu\t[ax][0-9]+,[ax][0-9]+,37,13} } } */
