/* PR rtl-optimization/34012 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

void bar (long int *);
void
foo (void)
{
  long int buf[10];
  buf[0] = 0x0808080808080808;
  buf[1] = 0x0808080808080808;
  buf[2] = 0x0808080808080808;
  buf[3] = 0x0808080808080808;
  buf[4] = 0x0808080808080808;
  buf[5] = 0x0808080808080808;
  buf[6] = 0x0808080808080808;
  buf[7] = 0x0808080808080808;
  buf[8] = 0x0808080808080808;
  buf[9] = 0x0808080808080808;
  bar (buf);
}

/* Check that CSE did its job and fwprop hasn't undone it.  */
/* { dg-final { scan-assembler-times "578721382704613384|0808080808080808" 1 } } */
