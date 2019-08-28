/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movq-1.c" for further details. */

/* assign memory to register */
volatile char mem;
void foo(void)
{
  register char dst = mem;
}
/* { dg-final { scan-assembler "ldb\[_s\\s\]+r\[0-9\]+,\\\[" } } */

