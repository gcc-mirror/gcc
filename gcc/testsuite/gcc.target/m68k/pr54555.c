/* PR rtl-optimization/54555
   Test that postreload does not shorten the load of small constants to
   use move.b instead of moveq.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "move\\.?b" } } */

void foo (void);
void bar (int a)
{
  if (a == 16 || a == 23) foo ();
  if (a == -110 || a == -128) foo ();
}
