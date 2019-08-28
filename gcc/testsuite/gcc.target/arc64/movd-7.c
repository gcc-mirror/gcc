/* { dg-do compile } */
/* { dg-options "-O1 -fomit-frame-pointer" } */

/* check "movd-1.c" for further details. */

/* assign memory to a memory */
int mem_dst, mem_src;
void foo(void)
{
  mem_dst = mem_src;
}
/* { dg-final { scan-assembler "ld.x\\s+r\[0-9\]+,\\\[" { target hs6x } } } */
/* { dg-final { scan-assembler-not "ext\[bhw\]\\s+" } } */
/* { dg-final { scan-assembler "st\[_s\\s\]+r\\d,\\\[" } } */

/* { dg-final { scan-assembler "ld\\s+r\[0-9\]+,\\\[" { target hs5x } } } */
