/* { dg-do run } */
/* { dg-options "-falign-labels=8" } */
/* { dg-skip-if "no label alignment > 2" { "pdp11-*-*" } } */

/* On ARMv7-A CPUs, this test resulted in incorrect code generation.
   The code generated for the switch statement expected the jump table
   to immediately follow the jump instruction, but -falign-labels
   caused the label preceding the table to be aligned.  */
/* M68K and fido only support -falign-labels argument <= 2.  */

volatile int x;

int main(void)
{
  int y;

  x = 0;

  switch(x)
    {
    case 0:
      y = 2 * x;
      break;
    case 1:
      y = -3 * x;
      break;
    case 2:
      y = x + 5;
      break;
    case 3:
      y = x - 7;
      break;
    default:
      break;
    }

  x = y;

  return 0;
}
