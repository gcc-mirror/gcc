/* { dg-do compile } */
/* { dg-options "-mel -mabi=32 -march=mips64r2 -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int DI __attribute__((mode(DI)));
typedef int SI __attribute__((mode(SI)));

__attribute__((mips16)) SI
f (SI x, SI y)
{
  return ((DI) x * y) >> 32;
}

/* { dg-final { scan-assembler-not "\tsw\t" } } */
