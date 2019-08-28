/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movh-1.c" for further details. */

/* assign memory to register */
volatile short mem;
void foo(void)
{
  register short dst = mem;
}
/* { dg-final { scan-assembler "ldh\[_s\\s\]+r\[0-9\]+,\\\[" } } */

