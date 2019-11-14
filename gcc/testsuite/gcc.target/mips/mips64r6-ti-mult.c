/* { dg-do compile } */
/* { dg-options "-mabi=64 -march=mips64r6" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } } */

typedef unsigned __int128 u128;
typedef unsigned long long u64;

u128
test (u64 a, u64 b)
{
  return (u128)a * (u128)b;
}

/* { dg-final { scan-assembler-not "__multi3" } } */
/* { dg-final { scan-assembler "dmul" } } */
/* { dg-final { scan-assembler "dmuhu" } } */
