/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movq-1.c" for further details. */

/* FIXME: with a 'volatile' this test generates an unnecessary extb */
/* assign memory to a memory */
volatile char mem_dst, mem_src;
void foo(void)
{
  mem_dst = mem_src;
}
/* { dg-final { scan-assembler "ldb\[_s\\s\]+r\[0-9\]+,\\\[" } } */
/* { dg-final { scan-assembler "stb\[_s\\s\]+r\\d,\\\[" } } */
