/* { dg-do compile } */
/* { dg-additional-options "-O3 --param max-loop-header-insns=2" } */

short int *ev;
int l4;

short int
a7 (void)
{
  short int uo = ev[0], ie = uo;

  for (int kp = 0; kp < l4; kp += 4)
    {
      uo += ev[kp + 1];
      ie += ev[kp];
    }

  return uo + ie;
}
