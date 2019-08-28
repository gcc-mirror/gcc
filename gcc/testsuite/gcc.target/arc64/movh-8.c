/* { dg-do compile } */
/* { dg-options "-O1 -fomit-frame-pointer" } */

/* check "movh-1.c" for further details. */

/* FIXME: with a 'volatile' this test generates an unnecessary sexh */
/* assign memory to a memory */
volatile short mem_dst, mem_src;
void foo(void)
{
  mem_dst = mem_src;
}
/* { dg-final { scan-assembler "ldh\\s+r\[0-9\]+,\\\[" } } */
/* { dg-final { scan-assembler-not "sexh" } } */
/* { dg-final { scan-assembler "sth\[_s\\s\]+r\\d,\\\[" } } */
