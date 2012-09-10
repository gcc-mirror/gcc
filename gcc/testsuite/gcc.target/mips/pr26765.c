/* PR target/pr26765
   This testcase used to trigger an unrecognizable insn.  */

/* { dg-do compile } */
/* { dg-options "-w" } */

__thread int *a = 0;

NOMIPS16 void foo (void)
{
  extern int *b;
  b = (int *) ((*a));
}
