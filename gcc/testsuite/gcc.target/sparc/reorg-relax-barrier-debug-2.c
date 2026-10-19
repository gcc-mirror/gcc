/* PR rtl-optimization/127808 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */
/* { dg-options "-O1 -fno-var-tracking -fcompare-debug" } */

/* relax_delay_slots must not delete a filled jump to the following label
   when a BARRIER and a debug insn separate the jump from that label.  */

int h;

void
foo (int b, int c)
{
  int i = b * 3;
  void *x = &&lab;
  if (b == 3)
    goto l2;
lab:
  __builtin_unreachable ();
l1:
  while (b < i)
    h = c;
l2:
  if (c)
    x = &&l1;
  goto *x;
}
