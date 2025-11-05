/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "maskeqz" } } */
/* { dg-final { scan-assembler "masknez" } } */

extern long lm, ln, lr;

void
test_and ()
{
  if (lm < 0)
    lr &= (1 << 16);
  lr += lm;
}
