/* Check that storing the 64-bit immediate to a volatile location is done
   with a single store.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned long long u64;

void bar (u64 *x)
{
  *(volatile u64 *)x = 0xabcdef10abcdef10ULL;
}

/* { dg-final { scan-assembler-times "str\tx..?, .*" 1 } } */
/* { dg-final { scan-assembler-not "str\tw..?, .*" } } */
