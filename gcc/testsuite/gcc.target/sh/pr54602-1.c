/* Verify that the delay slot is stuffed with register pop insns for normal
   (i.e. not interrupt handler) function returns.  If everything goes as
   expected we won't see any nop insns.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-not "nop" } } */

int test00 (int a, int b);

int
test01 (int a, int b, int c, int d)
{
  return test00 (a, b) + c;
}
