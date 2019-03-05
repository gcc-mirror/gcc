/* Test cases for Loongson EXT2 instructions.  */

/* { dg-do compile } */
/* { dg-options "-mloongson-ext2" } */

unsigned long long foo(unsigned long long x)
{
  return __builtin_ctzl (x);
}

/* { dg-final { scan-assembler "dctz\t" } } */
