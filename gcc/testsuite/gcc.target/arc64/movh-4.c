/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movh-1.c" for further details. */

/* register to memory */
short mem;
void foo(void)
{
  register short reg_short;
  mem = reg_short;
}
/* { dg-final { scan-assembler "sth\[_s\\s\]+r\[0-9\]+,\\\[" } } */
