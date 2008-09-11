/* { dg-do compile } */
/* { dg-options "-fpic -mxgot -mcpu=5206" } */
/* { dg-final { scan-assembler "foo@GOT,\%\[ad\]\[0-7\]" } } */

extern int foo;

int
bar (void)
{
  return foo;
}
