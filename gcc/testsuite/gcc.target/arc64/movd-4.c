/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movd-1.c" for further details. */

/* register to memory */
int mem;
void foo(void)
{
  register int reg_int;
  mem = reg_int;
}
/* { dg-final { scan-assembler "st\[_s\\s\]+r\[0-9\]+,\\\[" } } */
