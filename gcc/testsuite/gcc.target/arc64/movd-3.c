/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movd-1.c" for further details. */

/* assign memory to register */
volatile int mem;
void foo(void)
{
  register int dst = mem;
}
/* { dg-final { scan-assembler "ld\[_s\\s\]+r\[0-9\]+,\\\[" } } */

