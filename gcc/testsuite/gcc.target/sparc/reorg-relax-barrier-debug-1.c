/* PR rtl-optimization/127808 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */
/* { dg-options "-O2 -fno-var-tracking -fcompare-debug" } */

/* relax_delay_slots must not let a debug insn between a BARRIER and a
   jump target hide the BARRIER when it deletes the jump.  */

void
foo (int b, int c)
{
  void *x = &&lab;
  if (b < 5)
    goto l2;
l1:
  if (c < 0)
    goto l1;
l2:
  if (b)
    {
    lab:
      __builtin_unreachable ();
    }
  if (c)
    x = &&l1;
  goto *x;
}
