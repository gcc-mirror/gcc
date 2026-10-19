/* PR target/127900 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mbmi2" } */

typedef unsigned long long u64;

u64
mulh (u64 a, u64 b)
{
  return ((unsigned __int128) a * b) >> 64;
}

/* { dg-final { scan-assembler "\tmulx" } } */
/* { dg-final { scan-assembler-not "\tmulq" } } */
