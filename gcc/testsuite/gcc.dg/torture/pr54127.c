/* PR rtl-optimization/54127 */
/* { dg-do compile } */

extern void foo (void) __attribute__ ((__noreturn__));

void
bar (int x)
{
  if (x < 0)
    foo ();
  if (x == 0)
    return;
  __asm goto ("# %l[lab] %l[lab2]" : : : : lab, lab2);
lab:;
lab2:;
}
