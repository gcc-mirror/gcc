/* { dg-do compile } */
/* { dg-options "-O1 -fomit-frame-pointer" } */

/* check "movh-1.c" for further details. */

/* assign memory to a memory */
short mem_dst, mem_src;
void foo(void)
{
  mem_dst = mem_src;
}
/* { dg-final { scan-assembler "ldh\\s+r\[0-9\]+,\\\[" } } */
/* { dg-final { scan-assembler-not "ext\[bhw\]\\s+" } } */
/* { dg-final { scan-assembler "sth\[_s\\s\]+r\\d,\\\[" } } */
