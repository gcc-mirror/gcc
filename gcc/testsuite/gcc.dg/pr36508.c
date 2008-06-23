/* PR tree-optimization/36508 */
/* { dg-do compile } */
/* { dg-options "-O -ftree-pre" } */

void
foo (short *sp)
{
  int k;
  k = 1;
#define SP0 *sp++ = 1;
  while (1)
    {
      if (k > 6)
	break;
      SP0
      k++;
    }
  k = 1;
  while (1)
    {
      if (k > 6)
	break;
      SP0
      k++;
    }
#define SP1 SP0 SP0 SP0 SP0 SP0 SP0 SP0 SP0 SP0 SP0 SP0
#define SP2 SP1 SP1 SP1 SP1 SP1 SP1 SP1 SP1 SP1 SP1 SP1
  SP2
}
