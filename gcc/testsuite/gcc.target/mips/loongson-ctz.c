/* Test cases for Loongson EXT2 instructions.  */

/* { dg-do compile } */
/* { dg-options "-mloongson-ext2" } */

unsigned int foo(unsigned int x)
{
  return __builtin_ctz (x);
}

/* { dg-final { scan-assembler "ctz\t" } } */
