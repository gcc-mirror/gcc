/* This testcase ICEd on Alpha because ldq_u argument was not subject to
   small_symbolic_mem_operand splitting.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -fpic -mexplicit-relocs -mcpu=ev4" { target alpha*-*-* } } */

static char a;
char *b;

void foo (void)
{
  register char *c;

  c = b;
  *c = a;
}
