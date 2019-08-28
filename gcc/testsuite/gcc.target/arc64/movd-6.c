/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movd-1.c" for further details. */

/* assign immediate to a memory: this immediate cannot be   *
 * represented by 6-bit, hence stb w6, mem is not an option */
int mem;
void foo(void)
{
  mem = 0x40;    /* smallest 7-bit number */
  mem = 0x7F;    /* largest  7-bit number */
  mem = 0x80;    /* 128 */
  mem = -128;    /* smallest 8-bit number */
  mem = -33;     /* first negative number outside w6's range. else,
                    it would use something like 'sth -32, [@mem]'  */
}
/* { dg-final { scan-assembler "mov_s\\s+r\\d,64" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\\d,127" } } */
/* { dg-final { scan-assembler "mov_s\\s+r\\d,128" } } */
/* { dg-final { scan-assembler "mov\\s+r\\d,-128" } } */
/* { dg-final { scan-assembler "mov\\s+r\\d,-33" } } */
/* { dg-final { scan-assembler-times "st\[_s\\s\]+r\\d,\\\[" 5 } } */
