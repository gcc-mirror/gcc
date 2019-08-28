/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movq-1.c" for further details. */

/* register to memory */
char mem;
void foo(void)
{
  register char reg_char;
  mem = reg_char;
}
/* { dg-final { scan-assembler "stb\[_s\\s\]+r\[0-9\]+,\\\[" } } */
