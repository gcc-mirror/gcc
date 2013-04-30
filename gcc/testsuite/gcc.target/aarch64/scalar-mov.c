/* { dg-do compile } */
/* { dg-options "-g -mgeneral-regs-only" } */

void
foo (const char *c, ...)
{
  char buf[256];
  buf[256 - 1] = '\0';
}
